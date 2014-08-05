%{ open Ast %}

%token PLUS MINUS DIVIDE TIMES COMMA LEFT_BRACKET RIGHT_BRACKET
%token LEFT_PAREN RIGHT_PAREN EQ NEQ LEQ GEQ LT GT OR AND
%token ASSN_EQUAL LEFT_BRACE RIGHT_BRACE NEWLINE IN
%token FUNC IF ELSE FOR WHILE BREAK RETURN EOF
%token <int> INTLITERAL 
%token <string> IDENT 
%token <Ast.qual> QUAL


%nonassoc ELSE

%start yagl_program 
%type <Ast.yagl_program> yagl_program 

%%

yagl_program:
/* This tuple represents the AST for the entire yagl program. 
   The first list is the variable declaration list, the second is the function declaration list 
   and the last one is top level expressions list */ 											   
 | /*no code */                                   { ([], [], []) }
 | p = yagl_program; v_decs = variable_decl_list  { (v_decs @ first_elem p, second_elem p, third_elem p) }
 | p = yagl_program; top_code = statement         { (first_elem p, second_elem p, top_code @ third_elem p) }
 | p = yagl_program; f_decs = function_decl       { (first_elem p, f_decs @ second_elem p, third_elem p) }

variable_decl:
 | q = QUAL; id = IDENT; ASSN_EQUAL; e = expression { {qual=q;ident=id;rhs=e} };

variable_decl_list:
 | /* no variables declared */ { [] }
 | v1 = variable_decl_list; v2 = variable_decl { v2 :: v1 }

function_decl:
 | FUNC; name = IDENT; LEFT_PAREN; params = formal_params; RIGHT_PAREN; vars = variable_decl; s = statement_list
 { {fname=name;formals=params;locals=vars;body=s} }; 

formal_params:
 | /* could be a function that takes no arguments */ { [] } 
 | f1 = formal_params; q = QUAL; id = IDENT; f2 = formal_params; { ((q, id) :: f1) @ f2 }

statement:
 | e = expression NEWLINE                                                          { Expr(e) }
 | LEFT_BRACE; s = statement_list; RIGHT_BRACE                                     { Block(List.rev s) }
 | RETURN; e = expression                                                          { Return(e) }
 | BREAK NEWLINE                                                                   { Break } 
 | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement                      { If(e, s) }
 | FOR;LEFT_PAREN; q = QUAL; i = IDENT; IN; var = IDENT; LEFT_BRACE; s = statement { For(q, i, s) } 
 | WHILE; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement                   { While(e, s) } ;

statement_list:
 | /* empty block */ { [] } 
 | s1 = statement_list; s2 = statement { s2 :: s1 } 

expression:
 | /* no expression */                                              { NoExpr }
 | id = IDENT; LEFT_BRACKET; e = expression ;RIGHT_BRACKET          { ArrayIndex(id, e) }
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
 | FUNC; id = IDENT; LEFT_PAREN; args = func_arguments; RIGHT_PAREN { FuncCall(id, args) };

func_arguments:
 | /* No arguments passed to a function */      { [] }
 | arg_list = separated_list(COMMA, expression) { arg_list };
