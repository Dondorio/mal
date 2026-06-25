import gleam/dict.{type Dict}
import gleam/list
import gleam/result
import gleam/string
import mut_cell.{type MutCell}

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
  HashMap(Dict(MalType, MalType), meta: MalType)
  Atom(MutCell(MalType))

  Func(fn(List(MalType)) -> MalRet, is_macro: Bool, meta: MalType)
}

pub fn func(f: fn(List(MalType)) -> MalRet) {
  Func(f, False, Nil)
}

fn list_to_pairs(
  l: List(MalType),
  acc: List(#(MalType, MalType)),
) -> Result(List(#(MalType, MalType)), Error) {
  case l {
    [] -> Ok(acc)
    [String(_) as a, b, ..rest] | [Keyword(_) as a, b, ..rest] ->
      list_to_pairs(rest, [#(a, b), ..acc])
    _ -> Error(StrErr("failed to get hashmap pairs"))
  }
}

pub fn assoc(list) -> Result(Dict(_, _), Error) {
  use pairs <- result.try(list_to_pairs(list, []))
  // Reversed for (get { :a 1 :a 2 } :a) -> 2
  Ok(dict.from_list(list.reverse(pairs)))
}

pub fn hashmap(list) -> MalRet {
  assoc(list)
  |> result.map(fn(res) { HashMap(res, Nil) })
}

pub fn eq(a: MalType, b: MalType) -> Bool {
  case a, b {
    List(l, _), Vector(v, _)
    | Vector(l, _), List(v, _)
    | List(l, _), List(v, _)
    | Vector(l, _), Vector(v, _)
    -> seq_eq(l, v)
    HashMap(a, _), HashMap(b, _) -> hm_eq(a, b)
    _, _ -> a == b
  }
}

fn hm_eq(a: Dict(MalType, MalType), b: Dict(MalType, MalType)) -> Bool {
  case dict.size(a) == dict.size(b) {
    True -> {
      list.all(dict.to_list(a), fn(pairs) {
        let #(key, val) = pairs
        case dict.get(b, key) {
          Ok(val_b) -> eq(val, val_b)
          _ -> False
        }
      })
    }
    False -> False
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
        List(..) -> "list"
        Vector(..) -> "vector"
        HashMap(..) -> "hashmap"
        Func(..) -> "func"
        Atom(_) -> "atom"
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
  EvalLetPairOddCount(got: List(MalType))
  EvalApplyType(got: MalType)
  EvalSymbolNotFound(symbol: String)
  EvalFuncParamNotSymbol

  EnvToKey(ast: MalType)

  Throw(MalType)

  /// Used for one off errors
  StrErr(String)
}
