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
  type MalRet, type MalType, Builtin, HashMap, Int, List, Symbol, Vector,
}

pub fn main() -> Nil {
  let int_op = fn(with: fn(Int, Int) -> Result(Int, types.Error)) {
    Builtin(fn(l: List(MalType)) {
      case l {
        [a, b] -> {
          case a, b {
            Int(x), Int(y) -> with(x, y) |> result.map(fn(z) { Int(z) })
            // TODO
            // wrong type constructor function
            _, _ -> Error(types.EvalWrongType("", ""))
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
    Builtin(f) -> f(args)
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
    Symbol(sym) -> {
      case env.get(env, sym) {
        None -> Ok(#(ast, env))
        Some(val) -> Ok(#(val, env))
      }
    }
    List([l, ..rest], _) -> {
      let eval_args = fn() {
        list.try_fold(rest, #([], env), fn(acc, x) {
          use #(res, env) <- result.try(eval(x, acc.1))

          Ok(#([res, ..acc.0], env))
        })
        |> result.map(fn(o) { #(list.reverse(o.0), o.1) })
      }

      case l {
        Symbol("def!") ->
          case rest {
            [key, val] -> {
              use #(res, _) <- result.try(eval(val, env))
              use k <- result.try(env.ast_to_key(key))

              let env = env.set(env, k, res)

              Ok(#(res, env))
            }
            _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
          }
        Symbol("let*") -> {
          case rest {
            [pairs, closure] -> {
              case pairs {
                List(l, _) | Vector(l, _) -> {
                  use e <- result.try(let_special(l, [], env))
                  let new_env = env.into_outer(e)

                  use #(res, _) <- result.try(eval(closure, new_env))
                  Ok(#(res, env))
                }
                _ -> Error(types.EvalWrongType("list | vector", ""))
              }
            }
            _ -> Error(types.EvalWrongArgLen(2, list.length(rest)))
          }
        }

        Symbol(sym) -> {
          case env.get(env, sym) {
            None -> Error(types.EvalSymbolNotFound(sym))
            Some(a) -> {
              use #(args, env) <- result.try(eval_args())
              apply(a, args) |> result.map(fn(o) { #(o, env) })
            }
          }
        }
        _ -> {
          use #(args, env) <- result.try(eval_args())
          apply(l, args) |> result.map(fn(o) { #(o, env) })
        }
      }
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

fn let_special(pairs, acc, env) {
  case pairs {
    [key, val, ..rest] -> {
      use #(res, env) <- result.try(eval(val, env))
      use k <- result.try(env.ast_to_key(key))

      let env = env.set(env, k, res)

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
  use #(e, env) <- result.try(eval(r, env))

  Ok(#(print(e), env))
}
