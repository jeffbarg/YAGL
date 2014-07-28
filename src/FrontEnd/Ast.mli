type operator_token = Add of char
		    | Mult of char
		    | 

type syntax_token = Left_paren of char
		  | Right_paren of char
		  | Left_curly of char
		  | Right_curly of char
type stmt = Assignment of 

type expr = Bin_op of expr * expr 


type func_token = Function of string * expr array
