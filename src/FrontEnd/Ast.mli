type operator = Add 
	      | Mult 
	      | Sub 
	      | Div 

type variable = Variable of string 

type parameters = Params of expr array

type assign = Assn of variable * expr 

type expr = Binop of expr * operator * expr 
	  | FuncCall of variable * parameters


