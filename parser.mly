%{ open Ast %}

%token LEFT_PAREN RIGHT_PAREN LEFT_BRACE RIGHT_BRACE 
%token LEFT_BRACKET RIGHT_BRACKET COMMA 
%token NEWLINE EOF ASSN_EQUAL FUNC 
%token <string> IDENT STRING QUAL KEYWORD SIMPLE_STMT
%token <int> INTLITERAL 


%nonassoc ELSE

%start yagl_prog 
%type <Ast.program> yagl_prog

%%

yagl_prog:
  /* Making assumption that the first list is for variables 
     and the second is for function declarations, note that many 
     of the rules are left recursive 		*/
  /* Nothing right now */     { ([], []) }
 | yagl_prog var_declaration  { ($2 :: fst $1), snd $1 }
 | yagl_prog func_declaration { fst $1, ($2 :: snd $1) }

var_declaration:
  QUAL IDENT ASSN_EQUAL expr { var_decl{qual=$1;
					ident=$2;
					rhs=$4}}

func_declaration:
  FUNC IDENT LEFT_PAREN formal_opts RIGHT_PAREN LEFT_BRACE stmt_list RIGHT_BRACE
  {{fname=$2;
    formals=$4;
    locals=["123"];
    body=[If(1)]}}

stmt_list:
   RETURN expr { Return($2) }
 | IF LEFT_PAREN expr RIGHT_PARENT LEFT_BRACE stmt_list RIGHT_BRACE { If($3,  }

formal_opts:
  /* No arguments */ { [] } 
 | formal_list     { List.rev $1 } 

formal_list:
   IDENT { [$1]}
 | formal_list COMMA IDENT { $3 :: $1 } 

expr:
  IDENT LEFT_BRACKET expr RIGHT_BRACKET    { ArrayIndex($1, $3) }
| IDENT LEFT_PAREN actual_opts RIGHT_PAREN { FuncCall($1, $3)   } 

actual_opts:
  /* no arguments */  { [] }
 | actuals_list { List.rev $1 } 

actuals_list:
  expr { [$1] }
| actuals_list COMMA IDENT { $3 :: $1 } 

expression_opt:
/* nothing */ { } 
| expr { $1 } 
