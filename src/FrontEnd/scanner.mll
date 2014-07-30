{ 
  open Parser
}
let digit = ['0'-'9']
(* Need to add to language ref that variables must start with lower case *)
let ident = ['a'-'z']['a'-'z']*

rule main_entry = parse
  | [' ' '\r' '\t']                                         { main_entry lexbuf } 
  | ['#']                     { comment lexbuf }

  | '+' { PLUS }
  | '-' { MINUS }
  | '/' { DIVIDE }
  | '*' { TIMES }

  | ','                                                     { COMMA }
  | '['                                                     { LEFT_BRACKET } 
  | ']'                                                     { RIGHT_BRACKET } 
  | '('                                                     { LEFT_PAREN }
  | ')'                                                     { RIGHT_PAREN }

  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<" { LT }
  | ">" { GT }
  | "||" { OR }
  | "&&" { AND }

  | '='                                                     { ASSN_EQUAL } 
  | '{'                                                     { LEFT_BRACE } 
  | '}'                                                     { RIGHT_BRACE }
  | '\n'                                                    { NEWLINE } 


  | "if" { IF }
  | "else"                                                   { ELSE } 
  | "for"                                                   { FOR } 
  | "while"                                                   { WHILE } 

  | "break"                                                   { BREAK } 
  | "return" { RETURN }

  | "Array" { ARRAY }
  | "Dict" { DICT }
  | "Int" { INT }
  | "String" { STRING }

  | eof                                                     { EOF }
  | ident as ident                                          { IDENT(ident) }
  | digit+ as number                                        { INTLITERAL(int_of_string number) }
  
  (* need string literals *)
  

and comment = parse
  | ['\n'] { main_entry lexbuf }
  | _ { comment lexbuf }