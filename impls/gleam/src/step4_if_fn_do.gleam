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
  type MalRet, type MalType, Bool, Builtin, Func, HashMap, LazyFunc, List,
  Symbol, Vector,
}

pub fn main() -> Nil {
  let env = env.from_list(core.ns(), None)

  let assert Ok(#(_, env)) = rep("(def! not (fn* (a) (if a false true)))", env)

  let _ = loop(env)

  Nil
}

fn loop(env) {
  use input <- result.try(readline("user> "))

  let prn =
    input
    |> rep(env)
  case prn {
    Error(err) -> {
      printer.pr_err(err) |> io.println_error()
      loop(env)
    }
    Ok(#(str, env)) -> {
      io.println(str)

      loop(env)
    }
  }
}

fn apply(x: MalType, args: List(MalType)) -> MalRet {
  case x {
    Builtin(f) -> f(args)
    Func(f, _) -> f(args)
    LazyFunc(f) -> {
      use res <- result.try(f())
      apply(res, args)
    }
    _ -> Error(types.EvalApplyType(x))
  }
}

fn eval(ast: MalType, env: env.Env) -> Result(#(MalType, env.Env), types.Error) {
  case env.get(env, "DEBUG-EVAL") {
    None -> Nil
    Some(_) -> {
      io.println("EVAL: " <> printer.pr_str(ast, True))
    }
  }

  case ast {
    Symbol(sym) ->
      case env.get(env, sym) {
        None -> Error(types.EvalSymbolNotFound(sym))
        Some(val) ->
          case val {
            LazyFunc(f) -> result.map(f(), fn(o) { #(o, env) })
            _ -> Ok(#(val, env))
          }
      }
    List([first, ..rest], _) -> {
      eval_list(first, rest, env)
    }
    Vector(vec, _) -> {
      use #(res, env) <- result.try(
        list.try_fold(vec, #([], env), fn(acc, x) {
          use #(res, env) <- result.try(eval(x, acc.1))

          Ok(#([res, ..acc.0], env))
        })
        |> result.map(fn(o) { #(list.reverse(o.0), o.1) }),
      )

      Ok(#(Vector(res, types.Nil), env))
    }
    HashMap(hm, _) -> {
      use #(res, env) <- result.try(
        hm
        |> dict.to_list()
        |> list.try_fold(#([], env), fn(acc, x) {
          use #(res, env) <- result.try(eval(x.1, acc.1))

          let pair = #(x.0, res)
          Ok(#([pair, ..acc.0], env))
        }),
      )

      let ret = HashMap(dict.from_list(res), types.Nil)
      Ok(#(ret, env))
    }
    _ -> Ok(#(ast, env))
  }
}

fn eval_list(first, rest, env) {
  let eval_args = fn(env) {
    list.try_fold(rest, #([], env), fn(acc, x) {
      use #(res, env) <- result.try(eval(x, acc.1))

      Ok(#([res, ..acc.0], env))
    })
    |> result.map(fn(o) { #(list.reverse(o.0), o.1) })
  }

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
        [p, branch_true, ..branch_false] -> {
          use #(predicate, env) <- result.try(eval(p, env))

          use f <- result.try(case branch_false {
            [] -> Ok(types.Nil)
            [f] -> Ok(f)
            _ -> Error(types.EvalWrongArgLenRange(#(3, 4), list.length(rest)))
          })

          case predicate {
            Bool(False) | types.Nil -> {
              eval(f, env)
            }
            _ -> {
              eval(branch_true, env)
            }
          }
        }
        _ -> Error(types.EvalWrongArgLenRange(#(3, 4), list.length(rest)))
      }
    Symbol("do") -> {
      use #(res, env) <- result.try(eval_args(env))

      case list.last(res) {
        Error(_) -> Error(types.EvalWrongArgLenGreaterThan(1, 0))
        Ok(last) -> Ok(#(last, env))
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

          let func = fn(a) {
            use env <- result.try(
              env.into_outer(env)
              |> env.bind(param_names, a),
            )

            eval(body, env) |> result.map(fn(o) { o.0 })
          }

          Ok(#(Func(func, types.Nil), env))
        }
        [first, second] -> types.wrong_type_err("list, any", [first, second])
        _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
      }
    _ -> {
      use #(f, env) <- result.try(eval(first, env))
      use #(args, env) <- result.try(eval_args(env))

      apply(f, args) |> result.map(fn(o) { #(o, env) })
    }
  }
}

fn fix(f) {
  fn() { f(fix(f)) }
}

fn fix_with_args(f) {
  fn(x) { f(fix_with_args(f), x) }
}

fn def_special(key, val, env) {
  use k <- result.try(env.try_key(key))

  // Needed for recursion
  let gen_value: fn() -> Result(MalType, types.Error) =
    fix(fn(gen_value) {
      let env = env.set(env, k, LazyFunc(gen_value))
      eval(val, env) |> result.map(fn(o) { o.0 })
    })

  use v <- result.try(gen_value())
  Ok(#(v, env.set(env, k, v)))
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

fn let_special(pairs, body, original_env) {
  use p <- result.try(get_pairs(pairs, []))
  use pairs <- result.try(
    list.try_map(p, fn(x) {
      use k <- result.try(env.try_key(x.0))
      Ok(#(k, x.1))
    }),
  )

  let env = env.into_outer(original_env)

  let gen_value =
    fix_with_args(fn(gen_value, v) {
      let env =
        list.fold(pairs, env, fn(acc, p) {
          env.set(acc, p.0, LazyFunc(fn() { gen_value(p.1) }))
        })
      eval(v, env) |> result.map(fn(o) { o.0 })
    })

  use let_env <- result.try(
    list.try_fold(pairs, env, fn(acc, p) {
      use v <- result.try(gen_value(p.1))

      Ok(env.set(acc, p.0, v))
    }),
  )

  use #(res, _) <- result.try(eval(body, let_env))
  Ok(#(res, original_env))
}

fn read(str) {
  reader.read_str(str)
}

fn print(ast) {
  printer.pr_str(ast, True)
}

fn rep(str, env) {
  use r <- result.try(read(str))
  use #(e, env) <- result.try(eval(r, env))

  Ok(#(print(e), env))
}
