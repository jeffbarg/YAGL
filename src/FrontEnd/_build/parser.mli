type token =
  | LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | LEFT_BRACKET
  | RIGHT_BRACKET
  | COMMA
  | COLON
  | NEWLINE
  | EOF
  | IDENT of (string)
  | STRING of (string)
  | QUAL of (string)
  | KEYWORD of (string)
  | SIMPLE_STMT of (string)
  | OPER of (char)
  | INT of (int)

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.expr
