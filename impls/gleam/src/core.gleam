import argv
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mut_cell
import printer
import reader
import simplifile
import types.{
  type Error, type MalType, Atom, Bool, Func, Int, List, StrErr, String, Vector,
  func,
}

pub fn nth(list: List(a), n: Int) -> Option(a) {
  case n < 0 {
    True -> None
    False ->
      case list {
        [] -> None
        [x, ..rest] ->
          case n == 0 {
            True -> Some(x)
            False -> nth(rest, n - 1)
          }
      }
  }
}

pub fn ns() {
  let int_op = fn(with: fn(Int, Int) -> Result(MalType, Error)) {
    func(fn(l: List(MalType)) {
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

    // Bool
    #(">", int_op(fn(a, b) { Ok(Bool(a > b)) })),
    #(">=", int_op(fn(a, b) { Ok(Bool(a >= b)) })),
    #("<", int_op(fn(a, b) { Ok(Bool(a < b)) })),
    #("<=", int_op(fn(a, b) { Ok(Bool(a <= b)) })),
    #(
      "=",
      func(fn(args) {
        case args {
          [a, b, ..] -> Ok(Bool(types.eq(a, b)))
          _ -> Ok(Bool(False))
        }
      }),
    ),
    #(
      "list?",
      func(fn(args) {
        case args {
          [List(..), ..] -> Ok(Bool(True))
          _ -> Ok(Bool(False))
        }
      }),
    ),
    #(
      "empty?",
      func(fn(args) {
        case args {
          [List([], _)] | [Vector([], _)] -> Ok(Bool(True))
          [List(_, _)] | [Vector(_, _)] -> Ok(Bool(False))
          _ -> types.wrong_type_err("list | vector", args)
        }
      }),
    ),
    #(
      "atom?",
      func(fn(args) {
        case args {
          [Atom(_)] -> Ok(Bool(True))
          _ -> Ok(Bool(False))
        }
      }),
    ),
    #(
      "macro?",
      func(fn(args) {
        case args {
          [Func(is_macro: True, ..)] -> Ok(Bool(True))
          [_] -> Ok(Bool(False))
          _ -> types.wrong_type_err("any", args)
        }
      }),
    ),

    // String
    #(
      "pr-str",
      func(fn(args) {
        Ok(
          list.map(args, fn(x) { printer.pr_str(x, True) })
          |> string.join(" ")
          |> String,
        )
      }),
    ),
    #(
      "str",
      func(fn(args) {
        Ok(
          list.map(args, fn(x) { printer.pr_str(x, False) })
          |> string.join("")
          |> String,
        )
      }),
    ),

    // Seq
    #(
      "cons",
      func(fn(args) {
        case args {
          [head, List(tail, _)] | [head, Vector(tail, _)] ->
            Ok(List([head, ..tail], types.Nil))
          _ -> types.wrong_type_err("any, list | vector", args)
        }
      }),
    ),
    #(
      "concat",
      func(fn(args) {
        list.try_fold(args, [], fn(acc, x) {
          case x {
            List(l, _) | Vector(l, _) -> Ok(list.append(acc, l))
            _ -> types.wrong_type_err("...(list | vector)", args)
          }
        })
        |> result.map(fn(res) { List(res, types.Nil) })
      }),
    ),
    #(
      "first",
      func(fn(args) {
        case args {
          [List([first, ..], _)] | [Vector([first, ..], _)] -> Ok(first)
          [types.Nil] | [List(_, _)] | [Vector(_, _)] -> Ok(types.Nil)
          _ -> types.wrong_type_err("list | vector", args)
        }
      }),
    ),
    #(
      "rest",
      func(fn(args) {
        case args {
          [List([_, ..rest], _)] | [Vector([_, ..rest], _)] ->
            Ok(List(rest, types.Nil))
          [types.Nil] | [List(_, _)] | [Vector(_, _)] -> Ok(List([], types.Nil))
          _ -> types.wrong_type_err("list | vector", args)
        }
      }),
    ),
    #(
      "nth",
      func(fn(args) {
        case args {
          [List(data, _), Int(n)] | [Vector(data, _), Int(n)] ->
            nth(data, n)
            |> option.to_result(StrErr("index out of bounds"))
          _ -> types.wrong_type_err("list | vector, int", args)
        }
      }),
    ),

    // IO 
    #(
      "prn",
      func(fn(args) {
        list.map(args, fn(x) { printer.pr_str(x, True) })
        |> string.join(" ")
        |> io.println()
        Ok(types.Nil)
      }),
    ),
    #(
      "println",
      func(fn(args) {
        list.map(args, fn(x) { printer.pr_str(x, False) })
        |> string.join(" ")
        |> io.println()
        Ok(types.Nil)
      }),
    ),
    #(
      "slurp",
      func(fn(args) {
        case args {
          [String(path)] -> {
            case simplifile.read(path) {
              Ok(str) -> Ok(String(str))
              Error(err) ->
                Error(StrErr("failed to open file: " <> string.inspect(err)))
            }
          }
          _ -> types.wrong_type_err("string", args)
        }
      }),
    ),

    // Declare
    #("list", func(fn(args) { Ok(List(args, types.Nil)) })),
    #(
      "vec",
      func(fn(args) {
        case args {
          [List(l, _)] | [Vector(l, _)] -> Ok(Vector(l, types.Nil))
          _ -> types.wrong_type_err("list | vector", args)
        }
      }),
    ),
    #(
      "atom",
      func(fn(args) {
        case args {
          [data] -> Ok(Atom(mut_cell.new(data)))
          _ -> types.wrong_type_err("any", args)
        }
      }),
    ),

    // Atom 
    #(
      "deref",
      func(fn(args) {
        case args {
          [Atom(ref)] -> Ok(mut_cell.get(ref))
          _ -> types.wrong_type_err("atom", args)
        }
      }),
    ),
    #(
      "reset!",
      func(fn(args) {
        case args {
          [Atom(ref), data] -> {
            mut_cell.set(ref, data)
            Ok(data)
          }
          _ -> types.wrong_type_err("atom", args)
        }
      }),
    ),
    #(
      "swap!",
      func(fn(args) {
        case args {
          [Atom(ref), Func(f, ..), ..rest] ->
            mut_cell.try_update(ref, fn(data) { f([data, ..rest]) })
          _ -> types.wrong_type_err("atom, func", args)
        }
      }),
    ),

    // Other
    #(
      "count",
      func(fn(args) {
        case args {
          [List(l, _)] | [Vector(l, _)] -> Ok(Int(list.length(l)))
          _ -> Ok(Int(0))
        }
      }),
    ),
    #(
      "read-string",
      func(fn(args) {
        case args {
          [String(str)] -> reader.read_str(str)
          _ -> types.wrong_type_err("string", args)
        }
      }),
    ),
    #(
      "*ARGV*",
      List(
        argv.load().arguments
          // Ignore file path arg
          |> list.drop(1)
          |> list.map(fn(arg) { String(arg) }),
        types.Nil,
      ),
    ),
  ]
}
