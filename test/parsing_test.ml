open Error
open Printf

let prog_name =
  try Sys.argv.(1)
  with Invalid_argument "index out of bounds" ->
    Printf.kfprintf (fun _ -> exit 1) stderr
      "Expected a file name\n"

let prog_file =
  try open_in prog_name
  with Sys_error _ ->
    Printf.kfprintf (fun _ -> exit 1) stderr
      "File %s not existing\n" prog_name

let () =
  try
    let _ = Parser.prog Lexer.lexer (Lexing.from_channel prog_file) in
    printf "Program %s: parsing ok.\n" prog_name
  with
    Error (e, loc, msg) ->
    kfprintf (fun _ -> exit 1) stderr
      "Program %s: parsing failed.\n%s" prog_name (msg_of_error e loc msg)
