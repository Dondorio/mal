import gleam/dict
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/regexp
import gleam/result
import gleam/string
import types.{
  type Error, type MalType, Bool, HashMap, Int, Keyword, List, ReaderEmpyForm,
  ReaderEof, ReaderInvalidHashMap, String, Symbol, Vector,
}

pub fn read_str(str: String) {
  let res =
    tokenize(str)
    |> read_form()

  case res {
    Error(err) -> Error(err)
    Ok(ok) -> Ok(ok.0)
  }
}

fn tokenize(str: String) {
  let assert Ok(re) =
    regexp.from_string(
      "[\\s,]*"
      <> "("
      <> "~@"
      <> "|[\\[\\]{}()'`~^@]"
      <> "|\"(?:\\\\.|[^\\\"])*\"?"
      <> "|;.*"
      <> "|[^\\s\\[\\]{}('\"`,;)]*"
      <> ")",
    )

  // TODO remove the need for this filter
  // something to do with the regex or regexp.split
  regexp.split(re, str)
  |> list.filter(fn(x) { x != "" })
}

fn read_form(x: List(String)) {
  let expand = fn(rest: List(String), sym: String) {
    use #(f, new_rest) <- result.try(read_form(rest))

    let res = List([Symbol(sym), f], types.Nil)
    Ok(#(res, new_rest))
  }

  case x {
    ["(", ..rest] -> read_seq(rest, [], ")")
    ["[", ..rest] -> read_seq(rest, [], "]")
    ["{", ..rest] -> read_seq(rest, [], "}")
    ["'", ..rest] -> expand(rest, "quote")
    ["`", ..rest] -> expand(rest, "quasiquote")
    ["~", ..rest] -> expand(rest, "unquote")
    ["~@", ..rest] -> expand(rest, "splice-unquote")
    ["@", ..rest] -> expand(rest, "deref")

    ["^", ..rest] -> {
      use #(meta, r) <- result.try(read_form(rest))
      use #(data, new_rest) <- result.try(read_form(r))

      let res = List([Symbol("with-meta"), data, meta], types.Nil)
      Ok(#(res, new_rest))
    }

    [a, ..rest] -> {
      let res = read_atom(a)

      case res {
        Ok(o) -> Ok(#(o, rest))
        Error(e) -> Error(e)
      }
    }
    _ -> Error(ReaderEmpyForm)
  }
}

fn list_to_pairs(
  l: List(MalType),
  acc: List(#(MalType, MalType)),
) -> option.Option(List(#(MalType, MalType))) {
  case l {
    [a, b, ..rest] -> list_to_pairs(rest, [#(a, b), ..acc])
    [_] -> None
    [] -> Some(acc)
  }
}

fn read_seq(input: List(String), acc: List(MalType), closing: String) {
  case input {
    [] -> Error(ReaderEof(closing))
    [x, ..rest] if x == closing -> {
      case closing {
        ")" -> Ok(#(List(acc, types.Nil), rest))
        "]" -> Ok(#(Vector(acc, types.Nil), rest))
        "}" -> {
          let tuples = list_to_pairs(acc, [])

          case tuples {
            None -> Error(ReaderInvalidHashMap)
            Some(t) -> {
              let hm = dict.from_list(t)

              Ok(#(HashMap(hm, types.Nil), rest))
            }
          }
        }
        _ -> panic as "read_seq called with unknown closing symbol"
      }
    }
    _ -> {
      use #(atom, rest) <- result.try(read_form(input))
      read_seq(rest, list.append(acc, [atom]), closing)
    }
  }
}

fn use_parse_int(input: String, with: fn(Nil) -> Result(MalType, Error)) {
  let res = int.parse(input)
  case res {
    Error(_) -> with(Nil)
    Ok(int) -> Ok(Int(int))
  }
}

fn use_parse_str(input: String, with: fn(Nil) -> Result(MalType, Error)) {
  let assert Ok(str_re) = regexp.from_string("\"(?:\\\\.|[^\\\\\"])*\"")
  let is_string = regexp.check(str_re, input)

  case is_string {
    False -> with(Nil)
    True -> Ok(String(input |> string.drop_end(1) |> string.drop_start(1)))
  }
}

fn read_atom(input: String) -> Result(MalType, Error) {
  case input {
    "true" -> Ok(Bool(True))
    "false" -> Ok(Bool(False))
    _ -> {
      use _ <- use_parse_int(input)
      use _ <- use_parse_str(input)

      case input {
        ":" <> rest -> Ok(Keyword(rest))
        "\"" <> _ -> Error(ReaderEof("\""))
        _ -> Ok(Symbol(input))
      }
    }
  }
}
