import gleam/erlang/process.{type Subject}
import gleam/otp/actor

pub fn new(value: a) {
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

pub type Message(a) {
  Get(Subject(a))
  Set(a)
  Update(fn(a) -> a)
}

pub type MutCell(a) =
  actor.Started(process.Subject(Message(a)))

pub fn get(cell: MutCell(a)) {
  actor.call(cell.data, waiting: 10, sending: Get)
}

pub fn set(cell: MutCell(a), new: a) {
  actor.send(cell.data, Set(new))
  actor.continue(cell)
}

pub fn update(cell: MutCell(a), f: fn(a) -> a) {
  actor.send(cell.data, Update(f))
}
