open Ast

let file = if Array.length Sys.argv > 1 && Filename.check_suffix Sys.argv.(1) ".yagl"
	   then open_in Sys.argv.(1) 
	   else (print_endline "Must provide a YAGL source and the \
				file must end in .yagl";exit 1)
let _ =
  let lexbuf = Lexing.from_channel file in
  let program = Parser.program Scanner.token lexbuf in 
(* If we get to here, then the parser is okay! *)
  (* List.iter (fun a -> print_endline (a.id)) (fst program);
  List.iter (fun a -> print_endline (string_of_qual a.v_type)) (fst program);
  List.iter (fun a -> print_endline a.fname) (snd program); *)
  let verified = Semantic.verify program in 
(* Would be nice to have a static checker, so that we can reject int + string *)
  let compiled_code = Compile.translate verified in 
  CodeGen.generate compiled_code
