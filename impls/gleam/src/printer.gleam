import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import types.{Bool, HashMap, Int, Keyword, List, Nil, String, Symbol, Vector}

pub fn pr_err(err: types.Error) -> String {
  case err {
    types.ReaderEof(expected) -> "expected '" <> expected <> "', found EOF"
    types.ReaderInvalidHashMap ->
      "failed to construct hashmap: item count not divisible by 2"
    types.ReaderEmpyForm -> "read_form called with empty list"

    types.EvalWrongArgLen(expected, got) ->
      "expected "
      <> int.to_string(expected)
      <> " arguments, got "
      <> int.to_string(got)
    types.EvalWrongArgLenGreaterThan(expected, got) ->
      "expected at least "
      <> int.to_string(expected)
      <> " arguments, got "
      <> int.to_string(got)
    types.EvalWrongArgLenRange(expected, got) ->
      "expected "
      <> int.to_string(expected.0)
      <> " - "
      <> int.to_string(expected.1)
      <> " arguments, got "
      <> int.to_string(got)

    types.EvalWrongType(expected, got) ->
      "expected '" <> expected <> "', got '" <> got <> "'"

    types.EvalDivideByZero -> "can't divide by zero"
    types.EvalApplyType(x) -> "can't apply '" <> pr_str(x, True) <> "'"
    types.EvalSymbolNotFound(sym) -> "'" <> sym <> "' not found"
    types.EnvToKey(ast) -> "can't bind value to '" <> pr_str(ast, True) <> "'"
    types.EvalFuncParamNotSymbol -> "function parameter not of type symbol"
    types.StrErr(str) -> str
  }
}

fn escape_str(str, acc) {
  case str {
    "\n" <> rest -> escape_str(rest, acc <> "\\n")
    "\"" <> rest -> escape_str(rest, acc <> "\\\"")
    "\\" <> rest -> escape_str(rest, acc <> "\\\\")
    "" -> acc
    _ -> {
      case string.pop_grapheme(str) {
        Error(_) -> acc
        Ok(#(g, rest)) -> escape_str(rest, acc <> g)
      }
    }
  }
}

pub fn pr_str(ast: types.MalType, print_readability: Bool) -> String {
  case ast {
    Nil -> "nil"
    Int(int) -> int.to_string(int)
    String(str) ->
      case print_readability {
        False -> str
        True -> "\"" <> escape_str(str, "") <> "\""
      }
    Symbol(sym) -> sym
    Bool(bool) ->
      case bool {
        True -> "true"
        False -> "false"
      }
    Keyword(kwd) -> ":" <> kwd
    List(list, _) ->
      "("
      <> list.map(list, fn(x) { pr_str(x, print_readability) })
      |> string.join(" ")
      <> ")"
    Vector(arr, _) ->
      "["
      <> list.map(arr, fn(x) { pr_str(x, print_readability) })
      |> string.join(" ")
      <> "]"
    HashMap(hm, _) -> {
      let d =
        dict.fold(hm, [], fn(acc, key, val) {
          [
            pr_str(key, print_readability),
            pr_str(val, print_readability),
            ..acc
          ]
        })
        |> string.join(" ")

      "{" <> d <> "}"
    }
    types.Builtin(..) -> "#<builtin>"
    types.Func(..) -> "#<function>"
    types.LazyFunc(_) -> "#<lazy>"
  }
}
