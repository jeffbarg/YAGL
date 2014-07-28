type tokens = LEFT_PAREN
	    | RIGHT_PAREN 
	    | IDENT of string 
	    | OPER of char 
	    | ARRAY of tokens array 
	    (* Still might need to refactor this into 
               something more efficient *)
	    | DICT of (int * string) list 
	    | STRING of string 
	    | QUAL of string 
	    | KEYWORD of string 
	    | LEFT_BRACE 
	    | RIGHT_BRACE
	    | LEFT_BRACKET
	    | RIGHT_BRACKET
	    | INT of int 
	    | COMMA 
	    | PARAMETERS of (tokens * tokens) array 
	    | COLON 
	    (* Not sure if we need to do this? *)
	    | SIMPLE_STMT of string 
	    | NEWLINE
	    | EOF 

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
