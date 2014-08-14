type token =
  | SEMI
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | IN
  | LBRACK
  | RBRACK
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE
  | ASSIGN
  | FUNC
  | EQ
  | NEQ
  | LT
  | LEQ
  | GT
  | GEQ
  | RETURN
  | IF
  | ELSE
  | FOR
  | WHILE
  | INTLITERAL of (int)
  | INT
  | DICT
  | ARRAY
  | STRING
  | ID of (string)
  | STRINGLITERAL of (string)
  | EOF

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
