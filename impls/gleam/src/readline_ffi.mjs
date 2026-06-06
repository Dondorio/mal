import { Ok } from "./gleam.mjs";
import readlineSync from "readline-sync";

export function readline_func(prn) {
  const input = readlineSync.question(prn, {
    history: true,
  });

  return new Ok(input);
}
