open Ast 
(* Could be made more informative? *)
exception SemanticError of string 

type typ = TInt | TString
(* Not an exhaustive match *)
let rec infer_typ = function
  | LiteralInt _ -> TInt
  | LiteralString _ -> TString
  | Binop (e1, op, e2) ->
      let (t1, t2, ret_typ) = infer_op_typ op in
      if check_expr e1 t1 && check_expr e2 t2
      then ret_typ (* Make this more informative *)
      else raise (SemanticError "Type problem with: ")
  | _ -> TInt 

and infer_op_typ = function
  | Add | Mult | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq -> (TInt, TInt, TInt)

and check_expr e typ =
  let inf_typ = infer_typ e in
  typ = inf_typ

let infer_statement = function 
  (* Obviously more, but just focusing on low fruit *)
  | Expr(e) -> infer_typ e 
  (* Just a stop gap *)
  | _ -> TInt 

let infer_func_body = function 
  | {fname=_;formals=_;locals=_; body=a} -> List.fold_left  (* Only doing this for side effect *)
					      (fun accum s -> infer_statement s :: accum) 
					      [] 
					      a 
let var_check q rhs = match (q, rhs) with 
  | (Int, TInt) -> true 
	      
let infer_vars = function 
  | {id=_; v_type=Int; rhs=e} -> var_check Int (infer_typ e)

let verify (globals, funcs) = 
(* Basically doing these maps just for the possibility of a side effect going off,
   aka the exception, but if no exception, then we just give back what we are given *)
  let g_result = List.map infer_vars globals in 
  let f_result = List.map infer_func_body funcs in 
  (globals, funcs)
  
