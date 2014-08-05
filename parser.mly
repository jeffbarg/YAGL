%{ open Ast %}

%token PLUS MINUS DIVIDE TIMES COMMA LEFT_BRACKET RIGHT_BRACKET
%token LEFT_PAREN RIGHT_PAREN EQ NEQ LEQ GEQ LT GT OR AND
%token ASSN_EQUAL LEFT_BRACE RIGHT_BRACE NEWLINE 
%token FUNC IF ELSE FOR WHILE BREAK RETURN EOF
%token <int> INTLITERAL 
%token <string> IDENT QUAL 


%nonassoc ELSE

%start yagl_program 
%type <Ast.yagl_program> yagl_program 

%%

yagl_program:
/* This tuple represents the AST for the entire yagl program. 
   The first list is the variable declaration list, the second is the function declaration list 
   and the last one is top level expressions list */ 											   
 | /*no code */ { ([], [], []) };

expression:
 | /* no expression */                                     { NoExpr }
 | id = IDENT; LEFT_BRACKET; e = expression ;RIGHT_BRACKET { ArrayIndex(id, e) }
 | i = INTLITERAL                                          { Literal(i)}
 | id = IDENT                                              { Id(id) }
 | e1 = expression; PLUS; e2 = expression                  { BinOp(e1, Add, e2) }
 | e1 = expression; MINUS; e2 = expression                 { BinOp(e1, Sub, e2) }
 | e1 = expression; DIVIDE; e2 = expression                { BinOp(e1, Div, e2) }
 | e1 = expression; TIMES; e2 = expression                 { BinOp(e1, Mult, e2) }
 | e1 = expression; EQ; e2 = expression                    { BinOp(e1, Equal, e2) }
 | e1 = expression; NEQ; e2 = expression                   { BinOp(e1, Neq, e2) }
 | e1 = expression; LEQ; e2 = expression                   { BinOp(e1, Leq, e2) } 
 | e1 = expression; GEQ; e2 = expression                   { BinOp(e1, Geq, e2) }
 | e1 = expression; LT; e2 = expression                    { BinOp(e1, LessThan, e2) }
 | e1 = expression; GT; e2 = expression                    { BinOp(e1, GreaterThan, e2) }
 | e1 = expression; AND; e2 = expression                   { BinOp(e1, LogicalAnd, e2) }
 | e1 = expression; OR; e2 = expression                    { BinOp(e1, LogicalOr, e2) }
 | FUNC; id = IDENT; LEFT_PAREN; args = func_arguments; RIGHT_PAREN { FuncCall(id, args) };

func_arguments:
 | /* No arguments passed to a function */      { [] }
 | arg_list = separated_list(COMMA, expression) { arg_list };
