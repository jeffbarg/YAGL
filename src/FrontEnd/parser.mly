%{ 
open Terminals 
(* Not sure how to make symbol_table of growing size? 
   would I need to do a folding? *)
let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty

}%


%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE LEFT_BRACKET 
%token RIGHT_BRACKET COMMA COLON NEWLINE 

%token <string> IDENT STRING QUAL KEYWORD SIMPLE_STMT

%token <char> OPER

%token <tokens array> ARRAY

%token <(int * string) list> DICT

%token <int> INT 

%token <(tokens * tokens) array> PARAMETERS

