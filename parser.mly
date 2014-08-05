%{ open Ast %}
(* Note this parser is written to use menhir's features *)
%token PLUS MINUS DIVIDE TIMES COMMA LEFT_BRACKET RIGHT_BRACKET
%token LEFT_PAREN RIGHT_PAREN EQ NEQ LEQ GEQ LT GT OR AND
%token ASSN_EQUAL LEFT_BRACE RIGHT_BRACE NEWLINE IN
%token FUNC IF ELSE FOR WHILE BREAK RETURN EOF NOELSE
%token <int> INTLITERAL 
%token <string> IDENT 
%token <string> QUAL

%nonassoc ELSE NOELSE 

%start main
%type <Ast.yagl_program> main

%%

main:
 | yagl_program EOF { $1 } 

yagl_program:
(* This tuple represents the AST for the entire yagl program. 
   The first list is the variable declaration list, the second is the function declaration list 
   and the last one is top level expressions list *) 
 | (*no code *)                                   { ([], [], []) }
 | p = yagl_program; v_dec = variable_decl        { (v_dec :: (first_elem p), (second_elem p), (third_elem p)) }
 | p = yagl_program; top_code = statement         { (first_elem p, second_elem p, top_code :: third_elem p) }
(* | p = yagl_program; f_decs = function_decl       { (first_elem p, f_decs :: second_elem p, third_elem p) }; *)

variable_decl:
 | q = QUAL; id = IDENT; ASSN_EQUAL; e = expression { {qual=q;ident=id;rhs=e} };
(*
variable_decl_list:
 | (* no variables declared *)                 { [] }
 | v1 = variable_decl_list; v2 = variable_decl { v2 :: v1 }

function_decl:
 | FUNC; name = IDENT; LEFT_PAREN; params = formal_opts; RIGHT_PAREN; vars = variable_decl_list; s = statement_list
 { {fname=name;formals=List.rev params;locals=List.rev vars;body=s} }; 

formal_opts:
 | (* No arguments for a function *) { [] }
 | l = formal_params                 { List.rev l };

formal_params:
 | q = QUAL; id = IDENT                           { [(q, id)] }
 | l = formal_params; COMMA; q = QUAL; id = IDENT { (q, id) :: l }

statement:
 | e = expression NEWLINE                                                              { Expr(e) }
 | LEFT_BRACE; s = statement_list; RIGHT_BRACE                                         { Block(s) }
 | RETURN; e = expression; NEWLINE                                                     { Return(e) }
 | BREAK; NEWLINE                                                                      { Break } 
 | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement; NOELSE                  { If(e, s, Block([])) }
 | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s1 = statement; ELSE; s2 = statement   { If(e, s1, s2) } 
 | FOR;LEFT_PAREN; q = QUAL; i = IDENT; IN; var = IDENT; LEFT_BRACE; s = statement     { For(q, i, s) } 
 | WHILE; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement                       { While(e, s) } ; 

statement_list:
 | (* empty statements block *)                   { [] } 
 | s1 = statement_list; s2 = statement            { s2 :: s1 } 
 *)
expression:
 | (* no expression *)                                              { NoExpr }
(* | id = IDENT; LEFT_BRACKET; e = expression ;RIGHT_BRACKET          { ArrayIndex(id, e) }
 | i = INTLITERAL                                                   { Literal(i)}
 | id = IDENT                                                       { Id(id) }
 | e1 = expression; PLUS; e2 = expression                           { BinOp(e1, Add, e2) }
 | e1 = expression; MINUS; e2 = expression                          { BinOp(e1, Sub, e2) }
 | e1 = expression; DIVIDE; e2 = expression                         { BinOp(e1, Div, e2) }
 | e1 = expression; TIMES; e2 = expression                          { BinOp(e1, Mult, e2) }
 | e1 = expression; EQ; e2 = expression                             { BinOp(e1, Equal, e2) }
 | e1 = expression; NEQ; e2 = expression                            { BinOp(e1, Neq, e2) }
 | e1 = expression; LEQ; e2 = expression                            { BinOp(e1, Leq, e2) } 
 | e1 = expression; GEQ; e2 = expression                            { BinOp(e1, Geq, e2) }
 | e1 = expression; LT; e2 = expression                             { BinOp(e1, LessThan, e2) }
 | e1 = expression; GT; e2 = expression                             { BinOp(e1, GreaterThan, e2) }
 | e1 = expression; AND; e2 = expression                            { BinOp(e1, LogicalAnd, e2) }
 | e1 = expression; OR; e2 = expression                             { BinOp(e1, LogicalOr, e2) }
 | LEFT_PAREN; e = expression; RIGHT_PAREN                          { e } 
 | FUNC; id = IDENT; LEFT_PAREN; args = actuals_opts; RIGHT_PAREN   { FuncCall(id, args) };

actuals_opts:
 | (* No actual arguments passed *) { [] }
 | l = actuals_list                 { List.rev l }

actuals_list:
 | e = expression                          { [e] }
 | l = actuals_list; COMMA; e = expression { e :: l }
 *)
