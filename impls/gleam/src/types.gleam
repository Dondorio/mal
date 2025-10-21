import gleam/dict

pub type MalType {
  Nil
  Int(Int)
  Bool(Bool)
  String(String)
  Symbol(String)
  Keyword(String)
  List(List(MalType), MalType)
  Array(List(MalType), MalType)
  HashMap(dict.Dict(MalType, MalType), MalType)
}

pub type Error {
  ReaderEof(expected: String)
  ReaderInvalidHashMap
  ReaderEmpyForm
}
