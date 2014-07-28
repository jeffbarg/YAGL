type operator = Add 
	      | Mult 
	      | Sub 
	      | Div 

type variable = Variable of string 
 and assign = Assn of variable * expr 
 and expr = Binop of expr * operator * expr 
	  | FuncCall of variable * parameters
 and parameters = Params of expr array 




