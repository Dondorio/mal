import gleam/dict
import gleam/list
import gleam/string

pub type MalRet =
  Result(MalType, Error)

pub type MalType {
  Nil
  Int(Int)
  Bool(Bool)
  String(String)
  Symbol(String)
  Keyword(String)
  List(List(MalType), meta: MalType)
  Vector(List(MalType), meta: MalType)
  HashMap(dict.Dict(MalType, MalType), meta: MalType)

  Builtin(fn(List(MalType)) -> MalRet)
  Func(fn(List(MalType)) -> MalRet, meta: MalType)
  LazyFunc(fn() -> MalRet)
}

pub fn eq(a: MalType, b: MalType) -> Bool {
  case a, b {
    List(l, _), Vector(v, _)
    | Vector(l, _), List(v, _)
    | List(l, _), List(v, _)
    | Vector(l, _), Vector(v, _)
    -> seq_eq(l, v)
    _, _ -> a == b
  }
}

fn seq_eq(a: List(MalType), b: List(MalType)) {
  case a, b {
    [x, ..rest], [y, ..rest2] ->
      case eq(x, y) {
        False -> False
        True -> seq_eq(rest, rest2)
      }
    [], [] -> True
    _, _ -> False
  }
}

pub fn wrong_type_err(expected: String, got: List(MalType)) {
  let t =
    got
    |> list.map(fn(x) {
      case x {
        Nil -> "nil"
        Int(_) -> "int"
        Bool(_) -> "bool"
        String(_) -> "string"
        Symbol(_) -> "symbol"
        Keyword(_) -> "keyword"
        List(_, _) -> "list"
        Vector(_, _) -> "vector"
        HashMap(_, _) -> "hashmap"
        Builtin(_) -> "builtin"
        Func(_, _) -> "func"
        LazyFunc(_) -> "lazy"
      }
    })
    |> string.join(", ")

  Error(EvalWrongType(expected, t))
}

pub type Error {
  ReaderEof(expected: String)
  ReaderInvalidHashMap
  ReaderEmpyForm

  EvalWrongArgLen(expected: Int, got: Int)
  EvalWrongArgLenRange(expected: #(Int, Int), got: Int)
  EvalWrongArgLenGreaterThan(expected: Int, got: Int)
  EvalWrongType(expected: String, got: String)
  EvalDivideByZero
  EvalApplyType(got: MalType)
  EvalSymbolNotFound(symbol: String)
  EvalFuncParamNotSymbol

  EnvToKey(ast: MalType)

  /// Used for one off errors
  StrErr(String)
}
