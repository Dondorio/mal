import gleam/dict
import gleam/io
import gleam/list
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
    ]
    |> dict.from_list()

  let _ = loop(env)

  Nil
}

fn loop(env) {
  use input <- result.try(readline("user> "))

  let prn =
    input
    |> rep(env)
  case prn {
    Error(err) -> printer.pr_err(err) |> io.println_error()
    Ok(str) -> io.println(str)
  }

  loop(env)
}

fn apply(x: MalType, args: List(MalType)) -> MalRet {
  case x {
    Builtin(f) -> f(args)
    _ -> Error(types.EvalApplyType(x))
  }
}

fn eval(ast: MalType, env: dict.Dict(String, MalType)) -> types.MalRet {
  case ast {
    Symbol(sym) -> {
      case dict.get(env, sym) {
        Error(_) -> Ok(ast)
        Ok(val) -> Ok(val)
      }
    }
    List([l, ..rest], _) -> {
      use first <- result.try(eval(l, env))
      use args <- result.try(list.try_map(rest, fn(x) { eval(x, env) }))

      apply(first, args)
    }
    Vector(vec, _) ->
      vec
      |> list.try_map(fn(x) { eval(x, env) })
      |> result.map(fn(o) { Vector(o, types.Nil) })
    HashMap(hm, _) -> {
      use res <- result.try(
        hm
        |> dict.to_list()
        |> list.try_map(fn(x) {
          use e <- result.try(eval(x.1, env))
          Ok(#(x.0, e))
        }),
      )

      Ok(HashMap(dict.from_list(res), types.Nil))
    }
    _ -> Ok(ast)
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

  Ok(print(e))
}
