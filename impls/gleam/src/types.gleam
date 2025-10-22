import gleam/dict

pub type MalRet =
  Result(MalType, Error)

pub type MalType {
  Nil
  Int(Int)
  Bool(Bool)
  String(String)
  Symbol(String)
  Keyword(String)
  List(List(MalType), MalType)
  Vector(List(MalType), MalType)
  HashMap(dict.Dict(MalType, MalType), MalType)

  Builtin(fn(List(MalType)) -> MalRet)
}

pub type Error {
  ReaderEof(expected: String)
  ReaderInvalidHashMap
  ReaderEmpyForm
  EvalWrongArgLen(expected: Int, provided: Int)
  EvalWrongType(expected: String, provided: String)
  EvalDivideByZero
  EvalApplyType(provided: MalType)
}
