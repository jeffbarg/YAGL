type operator = Add 
	      | Mult 
	      | Sub 
	      | Div 

type variable     = Variable of string 
 and assign       = Assn of variable * expr 
 and expr         = Binop of expr * operator * expr 
	          | FuncCall of variable * parameters
 and parameters   = Params of expr array 
 and simple_statm = Simple_Statm of string 
 and compd_statm  = Compound_Statm of simple_statm array




