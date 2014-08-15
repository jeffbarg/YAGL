open Ast

exception SemanticError of string

(* Maybe add a mutable variable that holds...an error? *)
let verify (globals, funcs) = 
  let rec expr_check = function 
    | Binop(LiteralInt(e1), _, LiteralString(e2)) -> false 
    | Binop(LiteralString(e1), _, LiteralInt(e2)) -> false
    | Binop(LiteralInt(e1), _, LiteralInt(e)) -> true
    | Binop(l, _, LiteralInt(e2)) -> expr_check l
    | Binop(LiteralInt(e1), _, l) -> expr_check l
    | Id(a) -> true 
    | LiteralInt(x) -> true
    | LiteralString(x) -> true 

  in 
  let verify = function 
    (* Would like to make it smarter to that I know where the semantic error occured *)
    | {id=_; v_type=Int;rhs=LiteralInt(x)} -> true
    | {id=_; v_type=String;rhs=LiteralString(x)} -> true
    | _ -> false 
  in 
  let result = List.map verify globals in 
  if List.mem false result then raise (SemanticError("Type checker does not like you")) else (globals, funcs)

