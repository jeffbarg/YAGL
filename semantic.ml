open Ast

exception SemanticError of string
(* Verifying the global variables, like Int a = 123 String Hi = "hello" *)
let verify (globals, funcs) = 
  let verify = function 
    (* Might want to refactor this *)
    | {id=_; v_type=Int;rhs=LiteralInt(x)} -> true
    | {id=_; v_type=String;rhs=LiteralString(x)} -> true
    | _ -> false 
  in 
  let result = List.map verify globals in 
  if List.mem false result then raise (SemanticError("Type checker does not like you")) else (globals, funcs)

    

