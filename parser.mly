%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA COLON IN LBRACK RBRACK 
%token PLUS MINUS TIMES DIVIDE ASSIGN FUNC 
%token EQ NEQ LT LEQ GT GEQ
%token RETURN IF ELSE FOR WHILE 
%token <int> INTLITERAL
%token INT DICT ARRAY STRING
%token <string> ID STRINGLITERAL
%token EOF

%nonassoc NOELSE
%nonassoc ELSE
%left EQ NEQ
%left LT GT LEQ GEQ
%left PLUS MINUS
%left TIMES DIVIDE

%start program
%type <Ast.program> program

%%

program:
   /* nothing */ { [], [] }
 | program vdecl { ($2 :: fst $1), snd $1 }
 | program fdecl { fst $1, ($2 :: snd $1) } 

fdecl:
   FUNC ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
     { { fname = $2;
   formals = $4;
   locals = List.rev $7;
   body = List.rev $8 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    INT COLON ID                   { [(Int, $3)] }
  | STRING COLON ID                { [(String, $3)]}
  | ARRAY COLON ID                 { [(Array, $3)]}
  | DICT COLON ID                  { [(Dict, $3)]}
  | formal_list COMMA qual COLON ID { ($3, $5) :: $1 }

vdecl_list:
  |             { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:/* Need to come back to this, looks suspicious */
/* YAGL only knows string literals and int literals */
  | INT ID ASSIGN expr SEMI { {id=$2;
             v_type=Int;
             rhs=$4}}
  | INT ID SEMI { {id=$2;
             v_type=Int;
             rhs=LiteralInt(0)}}
  | STRING ID ASSIGN expr SEMI { {id=$2;
          v_type=String;
                                  rhs=$4}} 
  | STRING ID SEMI { {id=$2;
          v_type=String;
                                  rhs=LiteralString("")}} 

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN qual ID IN expr RPAREN stmt { For($3, $4, $6, $8) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }
  | ID ASSIGN expr SEMI { Variable($1, $3) }

qual:
  | ARRAY { Array }
  | INT { Int }
  | DICT { Dict }
  | STRING { String}

expr:
    INTLITERAL       { LiteralInt($1) }
  | STRINGLITERAL    { LiteralString($1) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater,  $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | ID LBRACK expr RBRACK { ArrayIndex($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
