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
  #import <stdio.h>
  #define DEBUG 0
  int main() {
    int num_globals = %i;
    int globals[num_globals];
    int stack[1024];
    char * str_stack[1024];
    char * empty_str = \"\";

    int i = 0;

    int fp = 0;
    int sp = 0;
    int pc = 0;

    int op1;
    int op2;
    int result;

    int new_fp;
    int new_pc;

    for (i = 0;i<1024;i++) {
      str_stack[i] = empty_str;
    }
    for (i = 0;i<1024;i++) {
      stack[i] = 0;
    }
    for (i = 0; i < num_globals; i++) {
      globals[i] = 0;
    }
  " prog.num_globals;

  let stack = Array.make 1024 0
  and globals = Array.make prog.num_globals 0 in

let label_counter = ref (-1) in
let rec execute_prog fp sp pc = 

  
  Array.iter (fun x ->
  incr label_counter;
  fprintf oc " if(DEBUG)printf(\"PC: %%i \\n \", pc); goto gotopc;LABEL%i:\n" !label_counter;
  match x with
    LitInt i -> 
    fprintf oc
      "stack[sp] = %i;sp++;pc++;" i;
  | StrInt s -> ()
  | Drp -> fprintf oc "sp--;pc++;"
  | Bin op -> 
    fprintf oc
      "op1 = stack[sp-2];\nop2=stack[sp-1];";
    (match op with
      Add -> fprintf oc "result=op1+op2;"
    | Sub -> fprintf oc "result=op1-op2;"
    | Mult -> fprintf oc "result=op1*op2;"
    | Div -> fprintf oc "result=op1/op2;"
    | Equal -> fprintf oc "result=op1==op2;"
    | Neq -> fprintf oc "result=op1!=op2;"
    | Less -> fprintf oc "result=op1<op2;"
    | Leq -> fprintf oc "result=op1<=op2;"
    | Greater -> fprintf oc "result=op1>op2;"
    | Geq -> fprintf oc "result=op1>=op2;");
    fprintf oc
      "stack[sp-2]=result;sp--;pc++;"
  | Lod i -> fprintf oc "stack[sp]=globals[%i];" i; fprintf oc "sp++;pc++;";
  | Str i -> fprintf oc "globals[%i]=stack[sp-1];" i; fprintf oc "pc++;";
  | Lfp i -> fprintf oc "stack[sp]=stack[fp+%i];" i ; fprintf oc "sp++;pc++;";
  | Sfp i -> fprintf oc "stack[fp+%i]=stack[sp-1];" i; fprintf oc "pc++;";
  | Jsr(-1) ->
    fprintf oc "%s"
        "printf(\"\\n%i\\n\", stack[sp-1]);pc++;"; 
  | Jsr i -> fprintf oc "stack[sp]=pc+1;sp++;pc=%i;" i;
  | Ent i -> fprintf oc "stack[sp]=fp;sp+=(%i+1);pc++;" i;
  | Rts i -> fprintf oc "new_fp = stack[fp];new_pc=stack[fp-1];stack[fp-1-%i]=stack[sp-1];fp=new_fp;sp=fp-%i;pc=new_pc;" i i;
  | Beq i -> fprintf oc "sp--;pc+=((stack[sp-1]==0)?%i:1);" i;
  | Bne i -> fprintf oc "sp--;pc+=((stack[sp-1]!=0)?%i:1);" i;
  | Bra i -> fprintf oc "pc+=%i;" i;
  | Hlt -> fprintf oc "goto END;";
) prog.text
in 
let retval = execute_prog 0 0 0 in
let rec switchstatements n s = (if n < 0 then s else switchstatements (n-1) (s ^ "case " ^ string_of_int n ^ ": goto LABEL" ^ string_of_int n ^ ";break;")) in
fprintf oc "gotopc:\nswitch(pc){%sdefault:printf(\"\\nERROR: %%i\",pc);break;}\nEND: ; }" (switchstatements !label_counter "");
close_out oc;
retval