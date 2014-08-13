open Ast

let file = Sys.argv.(1)

let _ =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let program = Parser.program Scanner.token lexbuf in 
(* If we get to here, then the parser is okay! *)
  List.iter (fun a -> print_endline (a.id)) (fst program);
  List.iter (fun a -> print_endline (string_of_qual a.v_type)) (fst program);
  List.iter (fun a -> print_endline a.fname) (snd program);

(*  let checked_code = Semantic.verify program in 
  "hello" *)
(* Would be nice to have a static checker, so that we can reject int + string *)
  let compiled_code = Compile.translate program in 
  CodeGen.generate compiled_code
