import gleam/dict.{type Dict}
import gleam/option.{type Option, None, Some}
import mut_cell.{type MutCell}
import types.{type Error, type MalType}

pub opaque type Env {
  Env(outer: Option(Env), data: MutCell(Dict(String, MalType)))
}

pub fn new(outer, data) -> Env {
  Env(outer, mut_cell.new(data))
}

pub fn get(env: Env, key: String) -> Option(MalType) {
  let hm = mut_cell.get(env.data)

  case dict.get(hm, key) {
    Error(_) -> {
      case env.outer {
        None -> None
        Some(e) -> get(e, key)
      }
    }
    Ok(data) -> Some(data)
  }
}

pub fn set(env: Env, key: String, val: MalType) {
  mut_cell.update(env.data, fn(data) {
    data
    |> dict.insert(key, val)
  })
}

pub fn bind(env: Env, from: List(String), to: List(MalType)) {
  case from {
    ["&", sym] -> Ok(set(env, sym, types.List(to, types.Nil)))
    [sym, ..from_rest] ->
      case to {
        [t, ..rest] -> {
          set(env, sym, t)
          bind(env, from_rest, rest)
        }
        [] -> Error(types.StrErr("can't bind env: not enough args provided"))
      }
    [] if to == [] -> Ok(Nil)
    _ -> Error(types.StrErr("can't bind env: too many args provided"))
  }
}

pub fn from_list(input: List(#(String, MalType)), outer: Option(Env)) -> Env {
  let data = dict.from_list(input)
  Env(outer, mut_cell.new(data))
}

pub fn from_dict(input: Dict(String, MalType), outer: Option(Env)) -> Env {
  Env(outer, mut_cell.new(input))
}

pub fn into_outer(outer: Env) -> Env {
  Env(Some(outer), mut_cell.new(dict.new()))
}

pub fn try_key(ast: MalType) -> Result(String, Error) {
  case ast {
    types.Symbol(sym) -> Ok(sym)
    _ -> Error(types.EnvToKey(ast))
  }
}
