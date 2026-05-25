import argv
import core
import env
import gleam/dict
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import printer
import reader
import readline.{readline}
import types.{
  type MalRet, type MalType, Bool, Builtin, Func, HashMap, List, Symbol, Vector,
}

pub fn main() -> Nil {
  let env = env.from_list(core.ns(), None)

  let assert Ok(_) =
    [
      "(def! not (fn* (a) (if a false true)))",
      "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
    ]
    |> list.try_map(fn(x) { rep(x, env) })

  env.set(
    env,
    "eval",
    Builtin(fn(args) {
      case args {
        [ast] -> eval(ast, env)
        _ -> types.wrong_type_err("any", args)
      }
    }),
  )

  case argv.load().arguments {
    [file, ..] -> {
      let _ = case rep("(load-file \"" <> file <> "\")", env) {
        Error(err) -> printer.pr_err(err) |> io.println_error()
        _ -> Nil
      }
    }
    _ -> {
      let _ = loop(env)
      Nil
    }
  }
}

fn loop(env) {
  use input <- result.try(readline("user> "))

  input
  |> rep(env)
  |> print_res()

  loop(env)
}

fn print_res(res) {
  case res {
    Ok(str) -> {
      io.println(str)
    }
    Error(err) -> {
      printer.pr_err(err) |> io.println_error()
    }
  }
}

fn apply(x: MalType, args: List(MalType)) -> MalRet {
  case x {
    Builtin(f) -> f(args)
    Func(f, _) -> f(args)
    _ -> Error(types.EvalApplyType(x))
  }
}

fn eval(ast: MalType, env: env.Env) -> MalRet {
  case env.get(env, "DEBUG-EVAL") {
    None | Some(Bool(False)) | Some(types.Nil) -> Nil
    _ -> {
      io.println("EVAL: " <> printer.pr_str(ast, True))
    }
  }

  case ast {
    Symbol(sym) ->
      env.get(env, sym)
      |> option.to_result(types.EvalSymbolNotFound(sym))
    List([first, ..rest], _) -> eval_list(first, rest, env)
    Vector(vec, _) ->
      list.try_map(vec, fn(x) { eval(x, env) })
      |> result.map(fn(x) { Vector(x, types.Nil) })
    HashMap(hm, _) -> {
      dict.to_list(hm)
      |> list.try_map(fn(x) {
        let key = x.0
        let val = x.1

        eval(val, env)
        |> result.map(fn(e) { #(key, e) })
      })
      |> result.map(fn(res) { HashMap(dict.from_list(res), types.Nil) })
    }
    _ -> Ok(ast)
  }
}

fn eval_list(first, rest, env) -> Result(MalType, types.Error) {
  case first {
    Symbol("def!") ->
      case rest {
        [key, val] -> def_special(key, val, env)
        _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
      }
    Symbol("let*") ->
      case rest {
        [pairs, body] ->
          case pairs {
            List(l, _) | Vector(l, _) -> {
              let_special(l, body, env)
            }
            _ -> types.wrong_type_err("list | vector", [pairs])
          }
        _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
      }
    Symbol("if") ->
      case rest {
        [predicate, branch_true, ..branch_false] -> {
          use predicate <- result.try(eval(predicate, env))

          use false_ast <- result.try(case branch_false {
            [] -> Ok(types.Nil)
            [f] -> Ok(f)
            _ -> Error(types.EvalWrongArgLenRange(#(3, 4), list.length(rest)))
          })

          case predicate {
            Bool(False) | types.Nil -> {
              eval(false_ast, env)
            }
            _ -> {
              eval(branch_true, env)
            }
          }
        }
        _ -> Error(types.EvalWrongArgLenRange(#(3, 4), list.length(rest)))
      }
    Symbol("do") -> {
      let do = list.take(rest, list.length(rest) - 1)
      use _ <- result.try(list.try_map(do, fn(x) { eval(x, env) }))

      case list.last(rest) {
        Error(_) -> Error(types.EvalWrongArgLenGreaterThan(1, 0))
        Ok(last) -> eval(last, env)
      }
    }
    Symbol("fn*") ->
      case rest {
        [List(params, _), body] | [Vector(params, _), body] -> {
          use param_names <- result.try(
            list.try_map(params, fn(x) {
              case x {
                Symbol(sym) -> Ok(sym)
                _ -> Error(types.EvalFuncParamNotSymbol)
              }
            }),
          )

          let func = fn(args) {
            let fn_env = env.into_outer(env)
            use _ <- result.try(env.bind(fn_env, param_names, args))

            eval(body, fn_env)
          }

          Ok(Func(func, types.Nil))
        }
        [_, _] as l -> types.wrong_type_err("list, any", l)
        _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
      }
    _ -> {
      use f <- result.try(eval(first, env))
      use args <- result.try(list.try_map(rest, fn(x) { eval(x, env) }))

      apply(f, args)
    }
  }
}

fn def_special(key, val, env) {
  use k <- result.try(env.try_key(key))
  use v <- result.try(eval(val, env))
  env.set(env, k, v)
  Ok(v)
}

fn get_pairs(list, acc) {
  case list {
    [a, b, ..rest] -> {
      get_pairs(rest, [#(a, b), ..acc])
    }
    [] -> Ok(list.reverse(acc))
    _ -> Error(types.StrErr("can't get bind pairs: count not divisible by 2"))
  }
}

fn let_special(pairs, body, env) {
  use pairs <- result.try(get_pairs(pairs, []))
  let let_env = env.into_outer(env)

  use _ <- result.try(
    list.try_map(pairs, fn(x) {
      use key <- result.try(env.try_key(x.0))
      use val <- result.try(eval(x.1, let_env))
      env.set(let_env, key, val)

      Ok(Nil)
    }),
  )

  eval(body, let_env)
}

fn read(str) {
  reader.read_str(str)
}

fn print(ast) {
  printer.pr_str(ast, True)
}

fn rep(str, env) {
  use r <- result.try(read(str))
  use e <- result.try(eval(r, env))

  Ok(print(e))
}
