import argv
import gleam/dict
import gleam/float
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import gleam/time/timestamp
import mut_cell
import printer
import reader
import readline
import simplifile
import types.{
  type Error, type MalType, Atom, Bool, Func, HashMap, Int, Keyword, List,
  StrErr, String, Symbol, Vector, func,
}

fn bool(b) {
  Ok(Bool(b))
}

fn f(name: String, fun) {
  #(name, func(fun))
}

// fn list_op()
fn int_op(name, with: fn(Int, Int) -> Result(MalType, Error)) {
  #(
    name,
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
    }),
  )
}

fn nth(list: List(a), n: Int) -> Option(a) {
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

fn do_str(args, print_readability) {
  let join = case print_readability {
    True -> " "
    False -> ""
  }

  Ok(
    list.map(args, fn(x) { printer.pr_str(x, print_readability) })
    |> string.join(join)
    |> String,
  )
}

fn str(args) {
  do_str(args, False)
}

fn pr_str(args) {
  do_str(args, True)
}

fn time_ms(args) {
  case args {
    [] -> {
      let time =
        timestamp.system_time()
        |> timestamp.to_unix_seconds()

      let int = float.truncate(time *. 1000.0)

      Ok(Int(int))
    }
    _ -> types.wrong_type_err("", args)
  }
}

pub fn ns() {
  [
    // Int
    int_op("+", fn(a, b) { Ok(Int(a + b)) }),
    int_op("-", fn(a, b) { Ok(Int(a - b)) }),
    int_op("*", fn(a, b) { Ok(Int(a * b)) }),
    int_op("/", fn(a, b) {
      case b == 0 {
        False -> Ok(Int(a / b))
        True -> Error(types.EvalDivideByZero)
      }
    }),

    // Bool
    int_op(">", fn(a, b) { bool(a > b) }),
    int_op(">=", fn(a, b) { bool(a >= b) }),
    int_op("<", fn(a, b) { bool(a < b) }),
    int_op("<=", fn(a, b) { bool(a <= b) }),
    f("=", fn(args) {
      case args {
        [a, b, ..] -> bool(types.eq(a, b))
        _ -> bool(False)
      }
    }),
    f("number?", fn(args) {
      case args {
        [Int(_)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("list?", fn(args) {
      case args {
        [List(..), ..] -> bool(True)
        _ -> bool(False)
      }
    }),
    f("vector?", fn(args) {
      case args {
        [Vector(..), ..] -> bool(True)
        _ -> bool(False)
      }
    }),
    f("sequential?", fn(args) {
      case args {
        [List(..), ..] | [Vector(..), ..] -> bool(True)
        _ -> bool(False)
      }
    }),
    f("empty?", fn(args) {
      case args {
        [List([], _)] | [Vector([], _)] -> bool(True)
        [List(_, _)] | [Vector(_, _)] -> bool(False)
        _ -> types.wrong_type_err("list | vector", args)
      }
    }),
    f("atom?", fn(args) {
      case args {
        [Atom(_)] -> bool(True)
        _ -> bool(False)
      }
    }),
    f("fn?", fn(args) {
      case args {
        [Func(is_macro: False, ..)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("macro?", fn(args) {
      case args {
        [Func(is_macro: True, ..)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("string?", fn(args) {
      case args {
        [String(_)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("symbol?", fn(args) {
      case args {
        [Symbol(_)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("nil?", fn(args) {
      case args {
        [types.Nil] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("true?", fn(args) {
      case args {
        [Bool(True)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("false?", fn(args) {
      case args {
        [Bool(False)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("keyword?", fn(args) {
      case args {
        [Keyword(_)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("map?", fn(args) {
      case args {
        [HashMap(..)] -> bool(True)
        [_] -> bool(False)
        _ -> types.wrong_type_err("any", args)
      }
    }),

    // String
    f("pr-str", pr_str),
    f("str", str),

    // Seq
    f("count", fn(args) {
      case args {
        [List(l, _)] | [Vector(l, _)] -> Ok(Int(list.length(l)))
        _ -> Ok(Int(0))
      }
    }),
    f("cons", fn(args) {
      case args {
        [head, List(tail, _)] | [head, Vector(tail, _)] ->
          Ok(List([head, ..tail], types.Nil))
        _ -> types.wrong_type_err("any, list | vector", args)
      }
    }),
    f("concat", fn(args) {
      list.try_fold(args, [], fn(acc, x) {
        case x {
          List(l, _) | Vector(l, _) -> Ok(list.append(acc, l))
          _ -> types.wrong_type_err("...(list | vector)", args)
        }
      })
      |> result.map(fn(res) { List(res, types.Nil) })
    }),
    f("conj", fn(args) {
      case args {
        [List(data, _), ..rest] ->
          Ok(List(list.append(list.reverse(rest), data), types.Nil))
        [Vector(data, _), ..rest] ->
          Ok(Vector(list.append(data, rest), types.Nil))
        _ -> types.wrong_type_err("any, list | vector", args)
      }
    }),
    f("first", fn(args) {
      case args {
        [List([first, ..], _)] | [Vector([first, ..], _)] -> Ok(first)
        [types.Nil] | [List(_, _)] | [Vector(_, _)] -> Ok(types.Nil)
        _ -> types.wrong_type_err("list | vector", args)
      }
    }),
    f("rest", fn(args) {
      case args {
        [List([_, ..rest], _)] | [Vector([_, ..rest], _)] ->
          Ok(List(rest, types.Nil))
        [types.Nil] | [List(_, _)] | [Vector(_, _)] -> Ok(List([], types.Nil))
        _ -> types.wrong_type_err("list | vector", args)
      }
    }),
    f("nth", fn(args) {
      case args {
        [List(data, _), Int(n)] | [Vector(data, _), Int(n)] ->
          nth(data, n)
          |> option.to_result(StrErr("index out of bounds"))
        _ -> types.wrong_type_err("list | vector, int", args)
      }
    }),
    f("map", fn(args) {
      case args {
        [Func(f, ..), List(data, _)] | [Func(f, ..), Vector(data, _)] ->
          list.try_map(data, fn(x) { f([x]) })
          |> result.map(fn(res) { List(res, types.Nil) })
        _ -> types.wrong_type_err("list | vector, func", args)
      }
    }),
    f("seq", fn(args) {
      case args {
        [List([], _)] | [Vector([], _)] | [String("")] | [types.Nil] ->
          Ok(types.Nil)
        [List(l, _)] | [Vector(l, _)] -> Ok(List(l, types.Nil))
        [String(str)] -> {
          let chars =
            string.to_graphemes(str) |> list.map(fn(char) { String(char) })

          Ok(List(chars, types.Nil))
        }
        _ -> types.wrong_type_err("list | vector | string | nil", args)
      }
    }),

    // HashMap
    f("assoc", fn(args) {
      case args {
        [HashMap(hm, _), ..rest] -> {
          types.assoc(rest)
          |> result.map(fn(res) { HashMap(dict.merge(hm, res), types.Nil) })
        }
        _ -> types.wrong_type_err("hashmap, ...any", args)
      }
    }),
    f("dissoc", fn(args) {
      case args {
        [HashMap(hm, _), ..keys] -> {
          Ok(HashMap(dict.drop(hm, keys), types.Nil))
        }
        [types.Nil, String(_)] | [types.Nil, Keyword(_)] -> Ok(types.Nil)
        _ -> types.wrong_type_err("hashmap, string | keyword", args)
      }
    }),
    f("get", fn(args) {
      case args {
        [HashMap(hm, _), String(_) as key]
        | [HashMap(hm, _), Keyword(_) as key] -> {
          case dict.get(hm, key) {
            Ok(ok) -> Ok(ok)
            _ -> Ok(types.Nil)
          }
        }
        [types.Nil, String(_)] | [types.Nil, Keyword(_)] -> Ok(types.Nil)
        _ -> types.wrong_type_err("hashmap, string | keyword", args)
      }
    }),
    f("contains?", fn(args) {
      case args {
        [HashMap(hm, _), String(_) as key]
        | [HashMap(hm, _), Keyword(_) as key] -> {
          case dict.has_key(hm, key) {
            b -> bool(b)
          }
        }
        _ -> types.wrong_type_err("hashmap, string | keyword", args)
      }
    }),
    f("keys", fn(args) {
      case args {
        [HashMap(hm, _)] -> {
          Ok(List(dict.keys(hm), types.Nil))
        }
        _ -> types.wrong_type_err("hashmap", args)
      }
    }),
    f("vals", fn(args) {
      case args {
        [HashMap(hm, _)] -> {
          Ok(List(dict.values(hm), types.Nil))
        }
        _ -> types.wrong_type_err("hashmap", args)
      }
    }),

    // IO 
    f("prn", fn(args) {
      list.map(args, fn(x) { printer.pr_str(x, True) })
      |> string.join(" ")
      |> io.println()
      Ok(types.Nil)
    }),
    f("println", fn(args) {
      list.map(args, fn(x) { printer.pr_str(x, False) })
      |> string.join(" ")
      |> io.println()
      Ok(types.Nil)
    }),
    f("slurp", fn(args) {
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
    f("readline", fn(args) {
      case args {
        [String(str)] -> {
          case readline.readline(str) {
            Ok(input) -> Ok(String(input))
            Error(_) -> Ok(types.Nil)
          }
        }
        _ -> types.wrong_type_err("string", args)
      }
    }),

    // Declare
    f("list", fn(args) { Ok(List(args, types.Nil)) }),
    f("vector", fn(args) { Ok(Vector(args, types.Nil)) }),
    f("vec", fn(args) {
      case args {
        [List(l, _)] | [Vector(l, _)] -> Ok(Vector(l, types.Nil))
        _ -> types.wrong_type_err("list | vector", args)
      }
    }),
    f("atom", fn(args) {
      case args {
        [data] -> Ok(Atom(mut_cell.new(data)))
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("symbol", fn(args) {
      case args {
        [String(str)] -> Ok(Symbol(str))
        _ -> types.wrong_type_err("string", args)
      }
    }),
    f("keyword", fn(args) {
      case args {
        [String(str)] | [Keyword(str)] -> Ok(Keyword(str))
        _ -> types.wrong_type_err("string | keyword", args)
      }
    }),
    f("hash-map", fn(args) { types.hashmap(args) }),

    // Atom 
    f("deref", fn(args) {
      case args {
        [Atom(ref)] -> Ok(mut_cell.get(ref))
        _ -> types.wrong_type_err("atom", args)
      }
    }),
    f("reset!", fn(args) {
      case args {
        [Atom(ref), data] -> {
          mut_cell.set(ref, data)
          Ok(data)
        }
        _ -> types.wrong_type_err("atom", args)
      }
    }),
    f("swap!", fn(args) {
      case args {
        [Atom(ref), Func(f, ..), ..rest] ->
          mut_cell.try_update(ref, fn(data) { f([data, ..rest]) })
        _ -> types.wrong_type_err("atom, func", args)
      }
    }),

    // Meta
    f("meta", fn(args) {
      case args {
        [List(meta:, ..)]
        | [Vector(meta:, ..)]
        | [HashMap(meta:, ..)]
        | [Func(meta:, ..)] -> Ok(meta)
        _ -> types.wrong_type_err("list | vector | hashmap | func", args)
      }
    }),
    f("with-meta", fn(args) {
      case args {
        [List(..) as l, meta] -> Ok(List(..l, meta:))
        [Vector(..) as l, meta] -> Ok(Vector(..l, meta:))
        [HashMap(..) as l, meta] -> Ok(HashMap(..l, meta:))
        [Func(..) as l, meta] -> Ok(Func(..l, meta: meta))
        _ -> types.wrong_type_err("list | vector | hashmap | func", args)
      }
    }),

    // Other
    f("apply", fn(args) {
      case args {
        [Func(f, ..), ..rest] -> {
          let middle = list.take(rest, list.length(rest) - 1)

          case list.last(rest) {
            Ok(List(l, _)) | Ok(Vector(l, _)) -> {
              let appended = list.append(middle, l)
              f(appended)
            }
            _ -> types.wrong_type_err("func, ...any, list | vector", args)
          }
        }
        _ -> types.wrong_type_err("func, ...any, list | vector", args)
      }
    }),
    f("read-string", fn(args) {
      case args {
        [String(str)] -> reader.read_str(str)
        _ -> types.wrong_type_err("string", args)
      }
    }),
    f("throw", fn(args) {
      case args {
        [data] -> Error(types.Throw(data))
        _ -> types.wrong_type_err("any", args)
      }
    }),
    f("time-ms", time_ms),
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
    #("*host-language*", String("gleam")),
  ]
}
