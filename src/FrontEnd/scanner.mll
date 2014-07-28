{ 
  open Parser
}
let digit = ['0'-'9']
(* Need to add to language ref that variables must start with lower case *)
let ident = ['a'-'z']['a'-'z']*

rule main_entry = parse
  | digit+ as number            { INT(int_of_string number)  }
  | ident as ident              { IDENT(ident) }
  | [' ' '\r' '\t']             { main_entry lexbuf } 
  | '+' | '-' | '/' | '*' as op { OPER(op) }
  | ','                         { COMMA }
  | '['                         { LEFT_BRACKET } 
  | ']'                         { RIGHT_BRACKET } 
  | '('                         { LEFT_PAREN }
  | ')'                         { RIGHT_PAREN }
  | '{'                                                     { LEFT_BRACE } 
  | '}'                                                     { RIGHT_BRACE }
  | '\n'                                                    { NEWLINE } 
  | "for" | "while" | "func" | "if" | "elif" | "else" as kw { KEYWORD(kw) } 
  | "Array" | "Dict" | "Int" as qual                        { QUAL(qual) }  
  | eof                         { EOF }

