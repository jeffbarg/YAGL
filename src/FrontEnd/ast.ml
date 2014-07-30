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

type variable     = Variable of string 
 and expr         = Binop of expr * operator * expr 
            | FuncCall of string 
      | Assn of variable * expr 
 and parameters   = Params of expr array 
 and simple_statm = Simple_Statm of string 
 and compd_statm  = Compound_Statm of simple_statm array
 and suite = Simple of simple_statm
  | Compound of compd_statm
 and type_spec = StringType
  | IntType
  | DictType
  | ArrayType
 and 