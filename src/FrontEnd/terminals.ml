type tokens = IDENT of string 
	    | OPER of char 
	    | ARRAY of tokens array 
	    (* Still might need to refactor this into 
               something more efficient *)
	    | DICT of (int * string) list 
	    | STRING of string 
	    | QUAL of string 
	    | KEYWORD of string 
	    | INT of int 
	    | PARAMETERS of (tokens * tokens) array 
	    | SIMPLE_STMT of string 

