

let main () = 
(* Flexible input, either as piped in or a stdin *)
  let lexbuf = if Sys.argv > 1 
	       then Sys.argv.(1)
	       else Lexing.from_channel stdin in 
  let output = Parser.prog Scanner.main_entry lexbuf in 
  print_endline "test"
