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
  type MalRet, type MalType, Func, HashMap, Int, List, Symbol, Vector,
}

pub fn main() -> Nil {
  let int_op = fn(with: fn(Int, Int) -> Result(Int, types.Error)) {
    types.func(fn(l: List(MalType)) {
      case l {
        [a, b] -> {
          case a, b {
            Int(x), Int(y) -> with(x, y) |> result.map(fn(z) { Int(z) })
            _, _ -> types.wrong_type_err("int, int", [a, b])
          }
        }
        _ -> Error(types.EvalWrongArgLen(2, list.length(l)))
      }
    })
  }

  let env =
    env.from_list(
      [
        #("+", int_op(fn(a, b) { Ok(a + b) })),
        #("-", int_op(fn(a, b) { Ok(a - b) })),
        #("*", int_op(fn(a, b) { Ok(a * b) })),
        #(
          "/",
          int_op(fn(a, b) {
            case b == 0 {
              False -> Ok(a / b)
              True -> Error(types.EvalDivideByZero)
            }
          }),
        ),
      ],
      None,
    )

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
    Func(f, ..) -> f(args)
    _ -> Error(types.EvalApplyType(x))
  }
}

fn eval(ast: MalType, env: env.Env) -> MalRet {
  case env.get(env, "DEBUG-EVAL") {
    None -> Nil
    Some(_) -> {
      io.println("EVAL: " <> printer.pr_str(ast, True))
    }
  }

  case ast {
    Symbol(sym) -> {
      case env.get(env, sym) {
        None -> Error(types.EvalSymbolNotFound(sym))
        Some(val) -> Ok(val)
      }
    }
    List([l, ..rest], _) -> {
      let eval_args = fn(env) { list.try_map(rest, fn(x) { eval(x, env) }) }

      case l {
        Symbol("def!") ->
          case rest {
            [key, val] -> {
              use res <- result.try(eval(val, env))
              use k <- result.try(env.try_key(key))

              env.set(env, k, res)

              Ok(res)
            }
            _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
          }
        Symbol("let*") -> {
          case rest {
            [pairs, closure] -> {
              case pairs {
                List(l, _) | Vector(l, _) -> {
                  let new_env = env.into_outer(env)
                  use e <- result.try(let_special(l, [], new_env))

                  use res <- result.try(eval(closure, e))
                  Ok(res)
                }
                _ -> Error(types.EvalWrongType("list | vector", ""))
              }
            }
            _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
          }
        }

        _ -> {
          use f <- result.try(eval(l, env))
          use args <- result.try(eval_args(env))

          apply(f, args)
        }
      }
    }
    Vector(vec, _) -> {
      use res <- result.try(
        list.try_fold(vec, [], fn(acc, x) {
          use res <- result.try(eval(x, env))

          Ok([res, ..acc])
        })
        |> result.map(fn(o) { list.reverse(o) }),
      )

      Ok(Vector(res, types.Nil))
    }
    HashMap(hm, _) -> {
      use res <- result.try(
        hm
        |> dict.to_list()
        |> list.try_fold([], fn(acc, x) {
          use res <- result.try(eval(x.1, env))

          let pair = #(x.0, res)
          Ok([pair, ..acc])
        }),
      )

      let ret = HashMap(dict.from_list(res), types.Nil)
      Ok(ret)
    }
    _ -> Ok(ast)
  }
}

fn let_special(pairs, acc, env) {
  case pairs {
    [key, val, ..rest] -> {
      use res <- result.try(eval(val, env))
      use k <- result.try(env.try_key(key))

      env.set(env, k, res)

      let_special(rest, [res, ..acc], env)
    }
    [] -> Ok(env)
    // TODO different err
    _ -> Error(types.EvalWrongArgLen(2, list.length(pairs)))
  }
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

  Ok(#(print(e), env))
}
