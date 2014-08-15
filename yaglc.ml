open Ast

let file = if Array.length Sys.argv > 1 && Filename.check_suffix Sys.argv.(1) ".yagl"
	   then open_in Sys.argv.(1) 
	   else (print_endline "Must provide a YAGL source and the \
				file must end in .yagl";exit 1)
let _ =
  let lexbuf = Lexing.from_channel file in
  try 
    let program = Parser.program Scanner.token lexbuf in
    let verified = Semantic.verify program in 
    let compiled_code = Compile.translate verified in 
    CodeGen.generate compiled_code
  with 
  | Parsing.Parse_error -> Printf.printf "Some kind of error between %S and %i\n"
					 (lexbuf.lex_curr_p.pos_fname)
					 (lexbuf.lex_curr_p.pos_lnum)
