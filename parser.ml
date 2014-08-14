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

open Parsing;;
let _ = parse_error;;
# 1 "parser.mly"
 open Ast 
# 43 "parser.ml"
let yytransl_const = [|
  257 (* SEMI *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* LBRACE *);
  261 (* RBRACE *);
  262 (* COMMA *);
  263 (* COLON *);
  264 (* IN *);
  265 (* LBRACK *);
  266 (* RBRACK *);
  267 (* PLUS *);
  268 (* MINUS *);
  269 (* TIMES *);
  270 (* DIVIDE *);
  271 (* ASSIGN *);
  272 (* FUNC *);
  273 (* EQ *);
  274 (* NEQ *);
  275 (* LT *);
  276 (* LEQ *);
  277 (* GT *);
  278 (* GEQ *);
  279 (* RETURN *);
  280 (* IF *);
  281 (* ELSE *);
  282 (* FOR *);
  283 (* WHILE *);
  285 (* INT *);
  286 (* DICT *);
  287 (* ARRAY *);
  288 (* STRING *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  284 (* INTLITERAL *);
  289 (* ID *);
  290 (* STRINGLITERAL *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\003\000\004\000\004\000\007\000\007\000\
\007\000\007\000\007\000\005\000\005\000\002\000\002\000\002\000\
\002\000\006\000\006\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\008\000\008\000\008\000\008\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\009\000\009\000\009\000\009\000\011\000\011\000\
\012\000\012\000\000\000"

let yylen = "\002\000\
\000\000\002\000\002\000\009\000\000\000\001\000\003\000\003\000\
\003\000\003\000\005\000\000\000\002\000\005\000\003\000\005\000\
\003\000\000\000\002\000\002\000\003\000\003\000\005\000\007\000\
\008\000\005\000\001\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\003\000\000\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\002\000\003\000\
\000\000\000\000\000\000\000\000\015\000\000\000\017\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\
\000\000\032\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\014\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\016\000\007\000\010\000\009\000\008\000\012\000\028\000\029\000\
\027\000\030\000\000\000\046\000\000\000\000\000\000\000\000\000\
\000\000\000\000\036\000\037\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\044\000\000\000\045\000\013\000\
\000\000\011\000\000\000\018\000\004\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\020\000\022\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\024\000\000\000\
\025\000"

let yydgoto = "\002\000\
\003\000\007\000\008\000\021\000\075\000\081\000\022\000\059\000\
\090\000\091\000\062\000\063\000"

let yysindex = "\040\000\
\000\000\000\000\028\255\017\255\025\255\026\255\000\000\000\000\
\061\255\000\255\003\255\125\255\000\000\009\255\000\000\009\255\
\059\255\062\255\064\255\068\255\067\255\079\255\009\255\000\000\
\004\255\000\000\175\255\189\255\055\255\060\255\065\255\066\255\
\098\255\012\000\037\000\009\255\009\255\000\000\009\255\009\255\
\009\255\009\255\009\255\009\255\009\255\009\255\009\255\009\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\105\255\000\000\097\000\112\255\113\255\085\000\
\048\255\048\255\000\000\000\000\109\000\109\000\095\255\095\255\
\095\255\095\255\234\254\088\255\000\000\009\255\000\000\000\000\
\012\255\000\000\097\000\000\000\000\000\009\255\124\255\126\255\
\132\255\203\255\000\000\050\255\217\255\009\255\012\000\009\255\
\000\000\000\000\000\000\049\000\094\255\061\000\077\255\127\255\
\077\255\118\255\115\255\000\000\077\255\142\255\000\000\077\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\146\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\146\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\147\255\000\000\000\000\
\119\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\162\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\018\255\000\000\164\255\000\000\
\141\255\163\255\000\000\000\000\002\255\083\000\239\255\245\255\
\011\000\017\000\063\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\045\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\090\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\093\000\000\000\000\000\000\000\086\000\000\000\076\000\
\242\255\200\255\000\000\000\000"

let yytablesize = 387
let yytable = "\027\000\
\013\000\028\000\038\000\015\000\038\000\036\000\005\000\038\000\
\035\000\006\000\023\000\038\000\037\000\023\000\014\000\084\000\
\085\000\016\000\038\000\038\000\049\000\061\000\064\000\049\000\
\065\000\066\000\067\000\068\000\069\000\070\000\071\000\072\000\
\073\000\074\000\086\000\087\000\024\000\088\000\089\000\024\000\
\001\000\025\000\026\000\004\000\025\000\026\000\106\000\050\000\
\108\000\009\000\050\000\023\000\111\000\084\000\098\000\113\000\
\005\000\010\000\011\000\006\000\041\000\042\000\012\000\083\000\
\018\000\029\000\018\000\018\000\030\000\033\000\031\000\093\000\
\086\000\087\000\032\000\088\000\089\000\024\000\023\000\100\000\
\084\000\102\000\025\000\026\000\034\000\018\000\018\000\050\000\
\018\000\018\000\018\000\023\000\051\000\023\000\023\000\018\000\
\018\000\052\000\053\000\086\000\087\000\054\000\088\000\089\000\
\024\000\039\000\040\000\041\000\042\000\025\000\026\000\076\000\
\023\000\023\000\077\000\023\000\023\000\023\000\078\000\033\000\
\082\000\033\000\023\000\023\000\033\000\094\000\104\000\095\000\
\033\000\033\000\033\000\033\000\033\000\096\000\107\000\033\000\
\033\000\033\000\033\000\033\000\033\000\034\000\109\000\034\000\
\112\000\051\000\034\000\110\000\005\000\006\000\034\000\034\000\
\034\000\017\000\018\000\019\000\020\000\034\000\034\000\034\000\
\034\000\034\000\034\000\035\000\047\000\035\000\048\000\080\000\
\035\000\092\000\101\000\000\000\035\000\035\000\035\000\038\000\
\000\000\000\000\000\000\035\000\035\000\035\000\035\000\035\000\
\035\000\039\000\040\000\041\000\042\000\049\000\000\000\043\000\
\044\000\045\000\046\000\047\000\048\000\000\000\000\000\039\000\
\040\000\041\000\042\000\097\000\000\000\043\000\044\000\045\000\
\046\000\047\000\048\000\000\000\000\000\039\000\040\000\041\000\
\042\000\099\000\000\000\043\000\044\000\045\000\046\000\047\000\
\048\000\000\000\000\000\039\000\040\000\041\000\042\000\000\000\
\000\000\043\000\044\000\045\000\046\000\047\000\048\000\040\000\
\000\000\040\000\000\000\000\000\040\000\041\000\000\000\041\000\
\040\000\000\000\041\000\000\000\000\000\000\000\041\000\040\000\
\040\000\040\000\040\000\040\000\040\000\041\000\041\000\041\000\
\041\000\041\000\041\000\042\000\000\000\042\000\000\000\000\000\
\042\000\043\000\000\000\043\000\042\000\000\000\043\000\000\000\
\000\000\000\000\043\000\042\000\042\000\042\000\042\000\042\000\
\042\000\043\000\043\000\043\000\043\000\043\000\043\000\060\000\
\055\000\056\000\057\000\058\000\000\000\000\000\000\000\039\000\
\040\000\041\000\042\000\103\000\000\000\043\000\044\000\045\000\
\046\000\047\000\048\000\039\000\040\000\041\000\042\000\105\000\
\000\000\043\000\044\000\045\000\046\000\047\000\048\000\039\000\
\040\000\041\000\042\000\000\000\000\000\043\000\044\000\045\000\
\046\000\047\000\048\000\039\000\000\000\039\000\000\000\000\000\
\039\000\000\000\000\000\000\000\039\000\000\000\079\000\039\000\
\040\000\041\000\042\000\039\000\039\000\043\000\044\000\045\000\
\046\000\047\000\048\000\039\000\040\000\041\000\042\000\000\000\
\000\000\043\000\044\000\045\000\046\000\047\000\048\000\039\000\
\040\000\041\000\042\000\000\000\000\000\000\000\000\000\045\000\
\046\000\047\000\048\000"

let yycheck = "\014\000\
\001\001\016\000\001\001\001\001\003\001\002\001\029\001\006\001\
\023\000\032\001\002\001\010\001\009\001\002\001\015\001\004\001\
\005\001\015\001\017\001\018\001\003\001\036\000\037\000\006\001\
\039\000\040\000\041\000\042\000\043\000\044\000\045\000\046\000\
\047\000\048\000\023\001\024\001\028\001\026\001\027\001\028\001\
\001\000\033\001\034\001\016\001\033\001\034\001\103\000\003\001\
\105\000\033\001\006\001\002\001\109\000\004\001\005\001\112\000\
\029\001\033\001\033\001\032\001\013\001\014\001\002\001\078\000\
\002\001\007\001\004\001\005\001\007\001\003\001\007\001\086\000\
\023\001\024\001\007\001\026\001\027\001\028\001\002\001\094\000\
\004\001\096\000\033\001\034\001\006\001\023\001\024\001\033\001\
\026\001\027\001\028\001\002\001\033\001\004\001\005\001\033\001\
\034\001\033\001\033\001\023\001\024\001\004\001\026\001\027\001\
\028\001\011\001\012\001\013\001\014\001\033\001\034\001\007\001\
\023\001\024\001\003\001\026\001\027\001\028\001\006\001\001\001\
\033\001\003\001\033\001\034\001\006\001\002\001\033\001\002\001\
\010\001\011\001\012\001\013\001\014\001\002\001\008\001\017\001\
\018\001\019\001\020\001\021\001\022\001\001\001\025\001\003\001\
\003\001\000\000\006\001\033\001\003\001\003\001\010\001\011\001\
\012\001\029\001\030\001\031\001\032\001\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\003\001\003\001\003\001\075\000\
\006\001\084\000\095\000\255\255\010\001\011\001\012\001\001\001\
\255\255\255\255\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\011\001\012\001\013\001\014\001\001\001\255\255\017\001\
\018\001\019\001\020\001\021\001\022\001\255\255\255\255\011\001\
\012\001\013\001\014\001\001\001\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\255\255\255\255\011\001\012\001\013\001\
\014\001\001\001\255\255\017\001\018\001\019\001\020\001\021\001\
\022\001\255\255\255\255\011\001\012\001\013\001\014\001\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\001\001\
\255\255\003\001\255\255\255\255\006\001\001\001\255\255\003\001\
\010\001\255\255\006\001\255\255\255\255\255\255\010\001\017\001\
\018\001\019\001\020\001\021\001\022\001\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\255\255\003\001\255\255\255\255\
\006\001\001\001\255\255\003\001\010\001\255\255\006\001\255\255\
\255\255\255\255\010\001\017\001\018\001\019\001\020\001\021\001\
\022\001\017\001\018\001\019\001\020\001\021\001\022\001\003\001\
\029\001\030\001\031\001\032\001\255\255\255\255\255\255\011\001\
\012\001\013\001\014\001\003\001\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\011\001\012\001\013\001\014\001\003\001\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\011\001\
\012\001\013\001\014\001\255\255\255\255\017\001\018\001\019\001\
\020\001\021\001\022\001\001\001\255\255\003\001\255\255\255\255\
\006\001\255\255\255\255\255\255\010\001\255\255\010\001\011\001\
\012\001\013\001\014\001\017\001\018\001\017\001\018\001\019\001\
\020\001\021\001\022\001\011\001\012\001\013\001\014\001\255\255\
\255\255\017\001\018\001\019\001\020\001\021\001\022\001\011\001\
\012\001\013\001\014\001\255\255\255\255\255\255\255\255\019\001\
\020\001\021\001\022\001"

let yynames_const = "\
  SEMI\000\
  LPAREN\000\
  RPAREN\000\
  LBRACE\000\
  RBRACE\000\
  COMMA\000\
  COLON\000\
  IN\000\
  LBRACK\000\
  RBRACK\000\
  PLUS\000\
  MINUS\000\
  TIMES\000\
  DIVIDE\000\
  ASSIGN\000\
  FUNC\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LEQ\000\
  GT\000\
  GEQ\000\
  RETURN\000\
  IF\000\
  ELSE\000\
  FOR\000\
  WHILE\000\
  INT\000\
  DICT\000\
  ARRAY\000\
  STRING\000\
  EOF\000\
  "

let yynames_block = "\
  INTLITERAL\000\
  ID\000\
  STRINGLITERAL\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    Obj.repr(
# 25 "parser.mly"
                 ( [], [] )
# 312 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 26 "parser.mly"
                 ( (_2 :: fst _1), snd _1 )
# 320 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.program) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'fdecl) in
    Obj.repr(
# 27 "parser.mly"
                 ( fst _1, (_2 :: snd _1) )
# 328 "parser.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'formals_opt) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'vdecl_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 31 "parser.mly"
     ( { fname = _2;
   formals = _4;
   locals = List.rev _7;
   body = List.rev _8 } )
# 341 "parser.ml"
               : 'fdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 37 "parser.mly"
                  ( [] )
# 347 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'formal_list) in
    Obj.repr(
# 38 "parser.mly"
                  ( List.rev _1 )
# 354 "parser.ml"
               : 'formals_opt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 41 "parser.mly"
                                   ( [(Int, _3)] )
# 361 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 42 "parser.mly"
                                   ( [(String, _3)])
# 368 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 43 "parser.mly"
                                   ( [(Array, _3)])
# 375 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 44 "parser.mly"
                                   ( [(Dict, _3)])
# 382 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'formal_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'qual) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 45 "parser.mly"
                                    ( (_3, _5) :: _1 )
# 391 "parser.ml"
               : 'formal_list))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser.mly"
                ( [] )
# 397 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'vdecl_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'vdecl) in
    Obj.repr(
# 49 "parser.mly"
                     ( _2 :: _1 )
# 405 "parser.ml"
               : 'vdecl_list))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 53 "parser.mly"
                            ( {id=_2;
             v_type=Int;
             rhs=_4})
# 415 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 56 "parser.mly"
                ( {id=_2;
             v_type=Int;
             rhs=LiteralInt(0)})
# 424 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 59 "parser.mly"
                               ( {id=_2;
          v_type=String;
                                  rhs=_4})
# 434 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 62 "parser.mly"
                   ( {id=_2;
          v_type=String;
                                  rhs=LiteralString("")})
# 443 "parser.ml"
               : 'vdecl))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
                   ( [] )
# 449 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 68 "parser.mly"
                   ( _2 :: _1 )
# 457 "parser.ml"
               : 'stmt_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 71 "parser.mly"
              ( Expr(_1) )
# 464 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 72 "parser.mly"
                     ( Return(_2) )
# 471 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'stmt_list) in
    Obj.repr(
# 73 "parser.mly"
                            ( Block(List.rev _2) )
# 478 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 74 "parser.mly"
                                            ( If(_3, _5, Block([])) )
# 486 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 75 "parser.mly"
                                            ( If(_3, _5, _7) )
# 495 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'qual) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 76 "parser.mly"
                                         ( For(_3, _4, _6, _8) )
# 505 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'stmt) in
    Obj.repr(
# 77 "parser.mly"
                                  ( While(_3, _5) )
# 513 "parser.ml"
               : 'stmt))
; (fun __caml_parser_env ->
    Obj.repr(
# 80 "parser.mly"
          ( Array )
# 519 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    Obj.repr(
# 81 "parser.mly"
        ( Int )
# 525 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
         ( Dict )
# 531 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    Obj.repr(
# 83 "parser.mly"
           ( String)
# 537 "parser.ml"
               : 'qual))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 86 "parser.mly"
                     ( LiteralInt(_1) )
# 544 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 87 "parser.mly"
                     ( LiteralString(_1) )
# 551 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 88 "parser.mly"
                     ( Id(_1) )
# 558 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 89 "parser.mly"
                     ( Binop(_1, Add,   _3) )
# 566 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 90 "parser.mly"
                     ( Binop(_1, Sub,   _3) )
# 574 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 91 "parser.mly"
                     ( Binop(_1, Mult,  _3) )
# 582 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 92 "parser.mly"
                     ( Binop(_1, Div,   _3) )
# 590 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 93 "parser.mly"
                     ( Binop(_1, Equal, _3) )
# 598 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 94 "parser.mly"
                     ( Binop(_1, Neq,   _3) )
# 606 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                     ( Binop(_1, Less,  _3) )
# 614 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 96 "parser.mly"
                     ( Binop(_1, Leq,   _3) )
# 622 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 97 "parser.mly"
                     ( Binop(_1, Greater,  _3) )
# 630 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 98 "parser.mly"
                     ( Binop(_1, Geq,   _3) )
# 638 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_opt) in
    Obj.repr(
# 99 "parser.mly"
                                 ( Call(_1, _3) )
# 646 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 100 "parser.mly"
                          ( ArrayIndex(_1, _3) )
# 654 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 101 "parser.mly"
                       ( _2 )
# 661 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 104 "parser.mly"
                  ( [] )
# 667 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_list) in
    Obj.repr(
# 105 "parser.mly"
                  ( List.rev _1 )
# 674 "parser.ml"
               : 'actuals_opt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 108 "parser.mly"
                            ( [_1] )
# 681 "parser.ml"
               : 'actuals_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals_list) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 109 "parser.mly"
                            ( _3 :: _1 )
# 689 "parser.ml"
               : 'actuals_list))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
