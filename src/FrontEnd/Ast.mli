(* type tokens = IDENT of string  *)
(* 	    | OPER of char  *)
(* 	    | ARRAY of tokens array  *)
(* 	    | DICT of (int * string) list  *)
(* 	    | STRING of string  *)
(* 	    | QUAL of string  *)
(* 	    | KEYWORD of string  *)
(* 	    | INT of int  *)
(* 	    | PARAMETERS of (tokens * tokens) array  *)
(* 	    | SIMPLE_STMT of string  *)

type operator = Add 
	      | Mult 
	      | Sub 
	      | Div 

type variable = Variable of string 

type expr = Binop of expr * operator * expr 
	  | Assn of variable * expr 
