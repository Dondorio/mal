import gleam/io
import gleam/result
import readline.{readline}

pub fn main() -> Nil {
  let _ = loop()
  Nil
}

fn loop() {
  use input <- result.try(readline("user> "))

  let _res = input |> rep()

  loop()
}

fn read(str) {
  str
}

fn eval(ast) {
  ast
}

fn print(ast) {
  io.println(ast)
  ast
}

fn rep(str) {
  read(str) |> eval() |> print()
}
