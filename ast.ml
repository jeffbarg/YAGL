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
	  | For of qual * string * stmt 
	  | While of expr * stmt 
	  | Break

type func_decl = {fname:string;
		  formals:qual * string list;
(* perhaps leave as list of var declarations?*)
		  locals:var_decl;
		  body:stmt list}

type yagl_program = var_decl list * func_decl list * stmt list 

(* Hideous but that's okay *)
let first_elem = function (first, second, third) -> first
let second_elem = function (first, second, third) -> second
let third_elem = function (first, second, third) -> third 
  

