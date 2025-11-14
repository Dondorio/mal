open Core

let filename = "/tmp/.malhistory"

let readline (prompt : string) =
  LNoise.history_load ~filename |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;
  let l = try LNoise.linenoise prompt with _ -> None in
  match l with
  | None -> None
  | Some line ->
      LNoise.history_add line |> ignore;
      LNoise.history_save ~filename |> ignore;
      Some line
