type token = LEFT_PAREN
	   | RIGHT_PAREN 
	   | IDENT of string 
	   | LEFT_BRACE 
	   | RIGHT_BRACE
	   | LEFT_BRACKET
	   | RIGHT_BRACKET
	   | STRING of string 
	   | INT of int 
	   | COMMA 
	   | KEYWORD of keyword
	   | PARAMETERS of (types * types) array 
	   | COLON 
	   (* Is this even necessary? *)
	   | COMMENT 
	   | NEWLINE

type types = INT of int 
	   | ARRAY of types array
	   (* Maybe refactor to something more efficient? *)
	   | DICT of (int * string) list
	   | STRING of string 

(* This needs work on like what makes up the blocks *)
type keyword = FOR 
	     | IF 
	     | ELIF
	     | ELSE 
(* not sure how to represent just yet *)
type statement = Simple of string

type expr = ADD of expr * expr
	  | MULT of expr * expr 
	  | DIV of expr * expr 
	  | SUB of expr * expr 
	  | CALL of 


(* type simple_statement *)
(* Remember that tokens just make groupings, 
   they don't have any meaning in themselves. 
   Hence a yagl function declaration would tokenize this:

   func make_graph(Int: cx, Int: cy, Int: r)
   { 
       addCircle(cx, cy, r)
   }

   into the following list of tokens
   
   [KEYWORD("func"); IDENT("make_graph");LEFT_PAREN; PARAMETERS; 
    LEFT_BRACE; need to come back *)
    
   
