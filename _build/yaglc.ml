let _ = let source_code = if Array.length Sys.argv > 1 && Filename.check_suffix Sys.argv.(1) ".yagl"
			  then open_in Sys.argv.(1) 
			  else (print_endline "Must provide a YAGL source and the\
					       file must end in .yagl";exit 1) in 
	let lexbuf = Lexing.from_channel source_code in 
	let yagl_program = Parser.yagl_program Scanner.main_entry lexbuf in  
	let byte_code = Compile.translate yagl_program in 
	CodeGen.generate_code byte_code 
