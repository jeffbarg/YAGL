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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 
  open Ast
  open Printf
(*  let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty *)
# 28 "parser.ml"
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
    0|]

let yytransl_block = [|
  266 (* IDENT *);
  267 (* STRING *);
  268 (* QUAL *);
  269 (* KEYWORD *);
  270 (* SIMPLE_STMT *);
  271 (* OPER *);
  272 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\000\000"

let yylen = "\002\000\
\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\002\000\000\000\001\000"

let yydgoto = "\002\000\
\004\000"

let yysindex = "\255\255\
\247\254\000\000\001\255\000\000\002\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000"

let yytablesize = 4
let yytable = "\001\000\
\003\000\005\000\000\000\006\000"

let yycheck = "\001\000\
\010\001\001\001\255\255\002\001"

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
                                   ( print_endline "double check "; FuncCall("hello"))
# 110 "parser.ml"
               : Ast.expr))
(* Entry expr *)
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
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.expr)
