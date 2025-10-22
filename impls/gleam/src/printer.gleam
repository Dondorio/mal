import gleam/dict
import gleam/int
import gleam/list
import gleam/string
import types.{
  Bool, HashMap, Int, Keyword, List, Nil, ReaderEmpyForm, ReaderEof,
  ReaderInvalidHashMap, String, Symbol, Vector,
}

pub fn prn_err(err: types.Error) {
  case err {
    ReaderEof(expected) -> "expected '" <> expected <> "', found EOF"
    ReaderInvalidHashMap ->
      "failed to construct hashmap: item count not divisible by 2"
    ReaderEmpyForm -> "read_form called with empty list"
    types.EvalWrongArgLen(expected, provided) ->
      "expected "
      <> int.to_string(expected)
      <> " arguments, got "
      <> int.to_string(provided)
    types.EvalWrongType(_expected, _provided) -> "todo"
    types.EvalDivideByZero -> "can't divide by zero"
    types.EvalApplyType(x) -> "can't apply '" <> pr_str(x, True) <> "'"
  }
}

pub fn pr_str(ast: types.MalType, print_readability: Bool) -> String {
  case ast {
    Nil -> "nil"
    Int(int) -> int.to_string(int)
    String(str) -> "\"" <> str <> "\""
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
    types.Builtin(_) -> "builtin"
  }
}
