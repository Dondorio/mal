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
  case dict.get(env.data, key) {
    Error(_) -> {
      case env.outer {
        None -> None
        Some(e) -> get(e, key)
      }
    }
    Ok(data) -> Some(data)
  }
}

pub fn set(env: Env, key: String, val: MalType) -> Env {
  env.data
  |> dict.insert(key, val)
  |> Env(env.outer, _)
}

pub fn bind(env: Env, from: List(String), to: List(MalType)) {
  case from {
    ["&", sym] -> Ok(set(env, sym, types.List(to, types.Nil)))
    [sym, ..from_rest] ->
      case to {
        [t, ..rest] -> {
          let env = set(env, sym, t)
          bind(env, from_rest, rest)
        }
        [] -> Error(types.StrErr("can't bind env: not enough args provided"))
      }
    [] if to == [] -> Ok(env)
    _ -> Error(types.StrErr("can't bind env: too many args provided"))
  }
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

pub fn try_key(ast: MalType) -> Result(String, Error) {
  case ast {
    types.Symbol(sym) -> Ok(sym)
    _ -> Error(types.EnvToKey(ast))
  }
}
