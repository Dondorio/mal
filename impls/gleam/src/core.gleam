import gleam/io
import gleam/list
import gleam/result
import gleam/string
import printer
import types.{type Error, type MalType, Bool, Builtin, Int, List, String, Vector}

pub fn ns() {
  let int_op = fn(with: fn(Int, Int) -> Result(MalType, Error)) {
    Builtin(fn(l: List(MalType)) {
      case l {
        [a, b] -> {
          case a, b {
            Int(x), Int(y) -> with(x, y) |> result.map(fn(z) { z })
            _, _ -> types.wrong_type_err("int, int", [a, b])
          }
        }
        _ -> Error(types.EvalWrongArgLen(2, list.length(l)))
      }
    })
  }

  [
    // Int
    #("+", int_op(fn(a, b) { Ok(Int(a + b)) })),
    #("-", int_op(fn(a, b) { Ok(Int(a - b)) })),
    #("*", int_op(fn(a, b) { Ok(Int(a * b)) })),
    #(
      "/",
      int_op(fn(a, b) {
        case b == 0 {
          False -> Ok(Int(a / b))
          True -> Error(types.EvalDivideByZero)
        }
      }),
    ),
    #(">", int_op(fn(a, b) { Ok(Bool(a > b)) })),
    #(">=", int_op(fn(a, b) { Ok(Bool(a >= b)) })),
    #("<", int_op(fn(a, b) { Ok(Bool(a < b)) })),
    #("<=", int_op(fn(a, b) { Ok(Bool(a <= b)) })),
    // Bool
    #(
      "=",
      Builtin(fn(args) {
        case args {
          [a, b, ..] -> Ok(Bool(types.eq(a, b)))
          _ -> Ok(Bool(False))
        }
      }),
    ),
    #(
      "list?",
      Builtin(fn(args) {
        case args {
          [List(..), ..] -> Ok(Bool(True))
          _ -> Ok(Bool(False))
        }
      }),
    ),
    #(
      "empty?",
      Builtin(fn(args) {
        case args {
          [List([], _), ..] | [Vector([], _), ..] -> Ok(Bool(True))
          [List(_, _), ..] | [Vector(_, _), ..] -> Ok(Bool(False))
          _ -> Error(types.EvalWrongArgLenGreaterThan(1, 0))
        }
      }),
    ),
    // String
    #(
      "pr-str",
      Builtin(fn(args) {
        Ok(
          list.map(args, fn(x) { printer.pr_str(x, True) })
          |> string.join(" ")
          |> String,
        )
      }),
    ),
    #(
      "str",
      Builtin(fn(args) {
        Ok(
          list.map(args, fn(x) { printer.pr_str(x, False) })
          |> string.join("")
          |> String,
        )
      }),
    ),
    // IO 
    #(
      "prn",
      Builtin(fn(args) {
        list.map(args, fn(x) { printer.pr_str(x, True) })
        |> string.join(" ")
        |> io.println()
        Ok(types.Nil)
      }),
    ),
    #(
      "println",
      Builtin(fn(args) {
        list.map(args, fn(x) { printer.pr_str(x, False) })
        |> string.join(" ")
        |> io.println()
        Ok(types.Nil)
      }),
    ),
    // Declare
    #("list", Builtin(fn(args) { Ok(List(args, types.Nil)) })),
    // Other
    #(
      "count",
      Builtin(fn(args) {
        case args {
          [List(l, _), ..] | [Vector(l, _), ..] -> Ok(Int(list.length(l)))
          _ -> Ok(Int(0))
        }
      }),
    ),
  ]
}
