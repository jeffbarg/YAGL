


let _ = let new_file = if Array.length Sys.argv > 1 
		       then open_in Sys.argv.(1) 
		       else (print_endline "Must provide a YAGL source\
					    file ending in .yagl";exit 1) in 
	let lexbuf = Lexing.from_channel new_file in 
	let yagl_program = Parser.program Scanner.main_entry lexbuf in  
	print_endline "hello"
	


