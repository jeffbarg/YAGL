{ 
  (* Not used at the moment *)
  open Terminals
  open Printf 
}
let digit = ['0'-'9']
(* Need to add to language ref that variables must start with lower case *)
let ident = ['a'-'z']['a'-'z']*

(* Remember that the only job of the lexer is to tokenize the input, that's it *)
rule main_entry = parse
  | digit+ as number            { INT(int_of_string number)  }
  | ident as ident              { IDENT(ident) }
  (* Fuck windows *)
  | [' ' '\r' '\t']             { main_entry lexbuf } 
  | '+' | '-' | '/' | '*' as op { OPER(op) }
  | ','                         { COMMA }
  | '('                         { LEFT_PAREN }
  | ')'                         { RIGHT_PAREN }
  | '{'                         { LEFT_BRACE } 
  | '}'                         { RIGHT_BRACE }
  (* Since we end a statement on a newline instead of ; *)
  | '\n'                        { NEWLINE } 
  (* Not sure if anything was left out *)
  | "for" | "while" | "func" | "if" | "elif" | "else" as kw { KEYWORD(kw) } 
  | "Array" | "Dict" | "Int" as qual { QUAL(qual) }  
  | eof                         { EOF }

{ 
  (* This is just for testing, will be removed later on, 
     want to see if we are getting the appropriate items *)
  let main () = 
    let cin = 
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin 
    in 
    let lexbuf = Lexing.from_channel cin in  
    main_entry lexbuf
(* since there are no printfs in the actions, 
   this won't give anything meaningful right now *)
  let _ = Printexc.print main () 
}
