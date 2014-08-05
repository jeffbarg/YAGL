type bstmt =
Lit of int (* Push a literal *)
 | Drp (* Discard a value *)
 | Bin of Ast.op (* Perform arithmetic on top of stack *)
 | Lod of int 
 | Str of int 
 | Lfp of int 
 | Sfp of int 
 | Jsr of int 
 | Ent of int 
 | Rts of int 
 | Beq of int 
 | Bne of int 
 | Bra of int 
 | Hlt

type prog = {
 num_globals : int; (* Number of global variables *) 
 text : bstmt array; (* Code for all the functions *)
}