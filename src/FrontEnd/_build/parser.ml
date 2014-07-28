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
  | ASSN_EQUAL
  | IDENT of (string)
  | STRING of (string)
  | QUAL of (string)
  | KEYWORD of (string)
  | SIMPLE_STMT of (string)
  | OPER of (char)
  | INT of (int)

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 
  open Ast
  open Printf
(*  let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty *)
# 29 "parser.ml"
let yytransl_const = [|
  257 (* LEFT_PAREN *);
  258 (* RIGHT_PAREN *);
  259 (* LEFT_BRACE *);
  260 (* RIGHT_BRACE *);
  261 (* LEFT_BRACKET *);
  262 (* RIGHT_BRACKET *);
  263 (* COMMA *);
  264 (* COLON *);
  265 (* NEWLINE *);
    0 (* EOF *);
  266 (* ASSN_EQUAL *);
    0|]

let yytransl_block = [|
  267 (* IDENT *);
  268 (* STRING *);
  269 (* QUAL *);
  270 (* KEYWORD *);
  271 (* SIMPLE_STMT *);
  272 (* OPER *);
  273 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\000\000"

let yylen = "\002\000\
\003\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\003\000\002\000\000\000\001\000"

let yydgoto = "\002\000\
\005\000"

let yysindex = "\001\000\
\253\254\000\000\255\254\000\255\000\000\000\000\002\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000"

let yytablesize = 8
let yytable = "\003\000\
\007\000\001\000\006\000\008\000\000\000\000\000\000\000\004\000"

let yycheck = "\003\001\
\001\001\001\000\004\001\002\001\255\255\255\255\255\255\011\001"

let yynames_const = "\
  LEFT_PAREN\000\
  RIGHT_PAREN\000\
  LEFT_BRACE\000\
  RIGHT_BRACE\000\
  LEFT_BRACKET\000\
  RIGHT_BRACKET\000\
  COMMA\000\
  COLON\000\
  NEWLINE\000\
  EOF\000\
  ASSN_EQUAL\000\
  "

let yynames_block = "\
  IDENT\000\
  STRING\000\
  QUAL\000\
  KEYWORD\000\
  SIMPLE_STMT\000\
  OPER\000\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    Obj.repr(
# 21 "parser.mly"
                                   ( print_endline _1; FuncCall("any string"))
# 113 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 22 "parser.mly"
                             ( print_endline "we got some braces"; FuncCall("some bs") )
# 119 "parser.ml"
               : Ast.expr))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
