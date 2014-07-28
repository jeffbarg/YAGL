%{ 
  open Ast
  open Printf
(*  let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty *)
%}

%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE LEFT_BRACKET RIGHT_BRACKET COMMA COLON NEWLINE EOF

%token <string> IDENT STRING QUAL KEYWORD SIMPLE_STMT

%token <char> OPER

%token <int> INT 

%start expr
%type <Ast.expr> expr

%%

expr:
    | IDENT LEFT_PAREN RIGHT_PAREN { print_endline "double check "; FuncCall("hello")}
  
