 type op = Add | Sub | Mult | Div 
    | Equal | Neq | Less | Leq | Greater | Geq

 and expr = LiteralInt of int
    | LiteralString of string
    | Id of string
    | Binop of expr * op * expr
    | Call of string * expr list
    | ArrayIndex of string * expr 
    | Noexpr

 and stmt = Block of stmt list
    | Expr of expr
    | Return of expr
    | If of expr * stmt * stmt
    | For of qual * string * expr * stmt 
    | While of expr * stmt
    | Variable of variable

 and qual = Dict | Array | Int | String 
           
 and variable = {id:string; v_type:qual;rhs:expr}
      
 and func_decl = {fname : string;
      formals : (qual * string) list;
      locals : variable list;
      body : stmt list}

type program = variable list * func_decl list

let string_of_qual = function 
  | Dict -> "Dict"
  | Array -> "Array"
  | String -> "String"
  | Int -> "Int"
