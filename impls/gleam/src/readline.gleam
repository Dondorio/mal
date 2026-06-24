@target(erlang)
import input.{input}

@target(erlang)
pub fn readline(prn: String) -> Result(String, Nil) {
  input(prn)
}

@target(javascript)
@external(javascript, "./readline_ffi.mjs", "readline_func")
pub fn readline(prn: String) -> Result(String, Nil)
