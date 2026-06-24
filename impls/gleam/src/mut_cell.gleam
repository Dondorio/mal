@target(erlang)
import gleam/erlang/process.{type Subject}
@target(erlang)
import gleam/otp/actor
import gleam/result

// ----- erlang -----
@target(erlang)
pub type MutCell(a) =
  actor.Started(process.Subject(Message(a)))

@target(erlang)
pub type Message(a) {
  Get(Subject(a))
  Set(a)
  Update(fn(a) -> a)
}

@target(erlang)
pub fn new(value: a) -> MutCell(a) {
  let handle_message = fn(state: a, message: Message(a)) -> actor.Next(
    a,
    Message(a),
  ) {
    case message {
      Get(reply) -> {
        actor.send(reply, state)
        actor.continue(state)
      }
      Set(i) -> {
        actor.continue(i)
      }
      Update(f) -> {
        actor.continue(f(state))
      }
    }
  }

  let assert Ok(actor) =
    actor.new(value)
    |> actor.on_message(handle_message)
    |> actor.start
  actor
}

@target(erlang)
pub fn get(cell: MutCell(a)) -> a {
  actor.call(cell.data, waiting: 10, sending: Get)
}

@target(erlang)
pub fn set(cell: MutCell(a), new: a) -> Nil {
  actor.send(cell.data, Set(new))
}

@target(erlang)
pub fn update(cell: MutCell(a), f: fn(a) -> a) -> Nil {
  actor.send(cell.data, Update(f))
}

// ----- javascript -----
@target(javascript)
pub type MutCell(a)

@target(javascript)
@external(javascript, "./mut_cell_ffi.mjs", "new_cell")
pub fn new(value: a) -> MutCell(a)

@target(javascript)
@external(javascript, "./mut_cell_ffi.mjs", "get")
pub fn get(cell: MutCell(a)) -> a

@target(javascript)
@external(javascript, "./mut_cell_ffi.mjs", "set")
pub fn set(cell: MutCell(a), new: a) -> Nil

@target(javascript)
@external(javascript, "./mut_cell_ffi.mjs", "update")
pub fn update(cell: MutCell(a), f: fn(a) -> a) -> Nil

// ----- common -----
pub fn try_update(cell: MutCell(a), f: fn(a) -> Result(a, e)) {
  let data = get(cell)
  use res <- result.try(f(data))
  set(cell, res)
  Ok(res)
}
