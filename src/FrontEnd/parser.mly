%{ 
open Terminals 
(* Not sure how to make symbol_table of growing size? 
   would I need to do a folding? *)
let symbol_table:(string, string) Hashtbl.t = Hashtbl.empty
