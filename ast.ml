type operator = Add 
              | Mult 
              | Sub 
              | Div
              | Neq
              | Equal
              | LessThan
              | Leq
              | Geq
              | GreaterThan
	      | LogicalAnd
	      | LogicalOr

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
(* Note that expressions are a type of statement *)		 
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

type yagl_program = var_decl list * func_decl list * expr list 

