type operator = Add 
              | Mult 
              | Sub 
              | Div
              | Neq
              | Equal
              | Less
              | Leq
              | Geq
              | Greater

type qual = Array 
	  | Dict 
	  | String 
	  | Int

type expr = Literal of int 
	  | Id of string 
	  | BinOp of expr * operator * expr
	  | FuncCall of string * expr list 
	  | ArrayIndex of string * expr
	  | NoExpr 

type var_decl = {qual:qual;
		 ident:string;
		 rhs:expr}
		 
type stmt = Block of stmt list
	  | Expr of expr 
	  | Return of expr
	  | If of expr * stmt 
	  | For of qual * string * expr * stmt 
	  | While of expr * stmt 

type func_decl = {fname:string;
		  formals:string list;
		  locals:string list;
		  body:stmt list}

type program = string list * func_decl list 

(* let rec string_of_expr = function  *)
(*   | Literal(a) -> string_of_int a *)
(*   | Id(s) -> s *)
(*   | BinOp(e1, o, e2) ->  *)
