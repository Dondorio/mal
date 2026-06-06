import input.{input}

@external(javascript, "./readline_ffi.mjs", "readline_func")
pub fn readline(prn: String) -> Result(String, Nil) {
  input(prn)
}
