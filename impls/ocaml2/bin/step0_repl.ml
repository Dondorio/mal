open Core

let read s = s
let eval ast = ast
let print ast = print_endline ast
let rep str = read str |> eval |> print

let rec loop () =
  let i =
    match Mal.Read_line.readline "user> " with Some s -> s | None -> exit 0
  in
  rep i;
  loop ()

let () = loop ()
