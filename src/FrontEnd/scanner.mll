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
  | digit+ as number            { INT(number)  }
  | ident as ident              { IDENT(ident) }
  | '+' | '-' | '/' | '*' as op { OPER(op) }
  | ','                         { COMMA }
  (* Not sure if anything was left out *)
  | "for" | "while" | "func" | "if" | "elif" | "else" as kw { KEYWORD(kw) } 
  | eof                         { }

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
  let _ = Printexc.print main () 
}
