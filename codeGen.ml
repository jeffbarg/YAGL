(* 
let file_name_helper given_string = (Filename.chop_extension given_string) ^ ".c"
(* Just a test example *)
let generate_code byte_code = let gen_file = open_out (file_name_helper Sys.argv.(1)) in 
			      output_string gen_file "#include<stdio.h>\nint main(int argc, char **argv)\n\
						      {\nprintf(\"hello world!\\n\");\nreturn 0;\n}";
			      flush gen_file 

 *)
open Ast
  
open Bytecode

open Printf
let file = "example.c"

let generate prog =
  let oc = open_out file in
  fprintf oc
  "
  int main() {
    int num_globals = %i;
    int globals[num_globals];
    int stack[1024];
    int i = 0;
    for (;i<1024;i++) {
      stack[i] = 0;
    }
    for (i = 0; i < num_globals; i++) {
      globals[i] = 0;
    }
  " prog.num_globals;

  let stack = Array.make 1024 0
  and globals = Array.make prog.num_globals 0 in
let rec execute_prog fp sp pc = match prog.text.(pc) with
    LitInt i -> 
fprintf oc
  "stack[%i] = %i" sp i;
   execute_prog fp (sp+1) (pc+1)
  | Drp -> execute_prog fp (sp-1) (pc+1)
  | Bin op -> let op1 = stack.(sp-2) and op2 = stack.(sp-1) in
    stack.(sp-2) <- (let boolean i = if i then 1 else 0 in
    match op with
      Add -> op1 + op2
    | Sub -> op1 - op2
    | Mult -> op1 * op2
    | Div -> op1 / op2
    | Equal -> boolean (op1 = op2)
    | Neq -> boolean (op1 != op2)
    | Less -> boolean (op1 < op2)
    | Leq -> boolean (op1 <= op2)
    | Greater -> boolean (op1 > op2)
    | Geq -> boolean (op1 >= op2)) ;
    execute_prog fp (sp-1) (pc+1)
  | Lod i -> stack.(sp) <- globals.(i) ; execute_prog fp (sp+1) (pc+1)
  | Str i -> globals.(i) <- stack.(sp-1) ; execute_prog fp sp (pc+1)
  | Lfp i -> stack.(sp) <- stack.(fp+i) ; execute_prog fp (sp+1) (pc+1)
  | Sfp i -> stack.(fp+i) <- stack.(sp-1) ; execute_prog fp sp (pc+1)
  | Jsr(-1) -> print_endline (string_of_int stack.(sp-1)) ;
    execute_prog fp sp (pc+1)
  | Jsr i -> stack.(sp) <- pc + 1 ; execute_prog fp (sp+1) i
  | Ent i -> stack.(sp) <- fp ; execute_prog sp (sp+i+1) (pc+1)
  | Rts i -> let new_fp = stack.(fp) and new_pc = stack.(fp-1) in
  stack.(fp-i-1) <- stack.(sp-1) ;
  execute_prog new_fp (fp-i) new_pc
  | Beq i -> execute_prog fp (sp-1)
  (pc + if stack.(sp-1) = 0 then i else 1)
  | Bne i -> execute_prog fp (sp-1)
  (pc + if stack.(sp-1) != 0 then i else 1)
  | Bra i -> execute_prog fp sp (pc+i)
  | Hlt -> ()

in print_endline "asdf"; close_out oc;
execute_prog 0 0 0