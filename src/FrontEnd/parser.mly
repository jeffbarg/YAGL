%{ 
  open Ast
  let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty
%}

%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE LEFT_BRACKET RIGHT_BRACKET COMMA COLON NEWLINE 

%token <string> IDENT STRING QUAL KEYWORD SIMPLE_STMT

%token <char> OPER

%token <int> INT 

%start prog
%type <Ast.expr> prog

%%

prog:
  | IDENT;LEFT_PAREN;parameters;RIGHT_PAREN         { } 
  | LEFT_BRACE; stmts = compd_statm; RIGHT_BRACE { }
  | LEFT_BRACKET; v1 = array_values; RIGHT_BRACKET  { }
  | i = INT { INT i } 
  | s = STRING { String s } 

array_values: 
  | (* nothing to show *) { [] } 
  |                       { } 

compound_stmts:
  | {}
