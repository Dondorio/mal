import gleam/io
import gleam/result
import printer
import reader
import readline.{readline}

pub fn main() -> Nil {
  let _ = loop()
  Nil
}

fn loop() {
  use input <- result.try(readline("user> "))

  let prn =
    input
    |> rep()
  case prn {
    Error(err) -> printer.prn_err(err) |> io.println_error()
    Ok(str) -> io.println(str)
  }

  loop()
}

fn read(str) {
  reader.read_str(str)
}

fn eval(ast) {
  ast
}

fn print(ast) {
  printer.pr_str(ast, True)
}

fn rep(str) {
  use re <- result.try(
    read(str)
    |> eval(),
  )

  Ok(print(re))
}
