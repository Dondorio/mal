import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import types.{type Error, type MalType}

pub opaque type Env {
  Env(outer: Option(Env), data: Dict(String, MalType))
}

pub fn new() -> Env {
  Env(None, dict.new())
}

pub fn get(env: Env, key: String) -> Option(MalType) {
  case env.outer {
    Some(e) -> get(e, key)
    None -> {
      env.data
      |> dict.get(key)
      |> option.from_result()
    }
  }
}

pub fn set(env: Env, key: String, val: MalType) -> Env {
  env.data
  |> dict.insert(key, val)
  |> Env(env.outer, _)
}

pub fn from_list(input: List(#(String, MalType)), outer: Option(Env)) -> Env {
  input
  |> dict.from_list()
  |> Env(outer, _)
}

pub fn from_dict(input: Dict(String, MalType), outer: Option(Env)) -> Env {
  input
  |> Env(outer, _)
}

pub fn into_outer(outer: Env) -> Env {
  Env(Some(outer), dict.new())
}

pub fn ast_to_key(ast: MalType) -> Result(String, Error) {
  case ast {
    types.Symbol(sym) -> Ok(sym)
    _ -> Error(types.EnvToKey(ast))
  }
}

pub fn key_to_ast(str: String) {
  types.Symbol(str)
}
