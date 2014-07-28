{ 
  open Parser
}
let digit = ['0'-'9']
(* Need to add to language ref that variables must start with lower case *)
let ident = ['a'-'z']['a'-'z']*

rule main_entry = parse
  | digit+ as number                                        { INT(int_of_string number) }
  | ident as ident                                          { IDENT(ident) }
  | [' ' '\r' '\t']                                         { main_entry lexbuf } 
  | '+' | '-' | '/' | '*' as op                             { OPER(op) }
  | ','                                                     { COMMA }
  | '['                                                     { LEFT_BRACKET } 
  | ']'                                                     { RIGHT_BRACKET } 
  | '('                                                     { LEFT_PAREN }
  (* Need to add the relational operators and the logical operators && || *)
  | ')'                                                     { RIGHT_PAREN }
  | '='                                                     { ASSN_EQUAL } 
  | '{'                                                     { LEFT_BRACE } 
  | '}'                                                     { RIGHT_BRACE }
  | ['#']['A'-'Z' 'a'-'z' '0'-'9' '_']*                     { main_entry lexbuf }
  | '\n'                                                    { NEWLINE } 
  | "for" | "while" | "func" | "if" | "elif" | "else" as kw { KEYWORD(kw) } 
  | "Array" | "Dict" | "Int" as qual                        { QUAL(qual) }
  | eof                                                     { EOF }






