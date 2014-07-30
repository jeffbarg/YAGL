%{ 
  open Ast
  open Printf
(*  let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty *)
%}

%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE LEFT_BRACKET RIGHT_BRACKET COMMA COLON NEWLINE EOF ASSN_EQUAL
%token <string> IDENT STRING QUAL KEYWORD SIMPLE_STMT
%token <int> INTLITERAL 

%nonassoc ELSE

%start prog
%type <Ast.expr> prog

%%

prog:
    | IDENT LEFT_PAREN RIGHT_PAREN { print_endline $1; FuncCall("any string")}
    | LEFT_BRACE RIGHT_BRACE { print_endline "we got some braces"; FuncCall("some bs") }