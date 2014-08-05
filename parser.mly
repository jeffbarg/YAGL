%{ open Ast %}
(* Note this parser is written to use menhir's features *)
%token PLUS MINUS DIVIDE TIMES COMMA LEFT_BRACKET RIGHT_BRACKET
%token LEFT_PAREN RIGHT_PAREN EQ NEQ LEQ GEQ LT GT OR AND
%token ASSN_EQUAL LEFT_BRACE RIGHT_BRACE NEWLINE IN
%token FUNC IF ELSE FOR WHILE BREAK RETURN EOF NOELSE
%token <int> INTLITERAL 
%token <string> IDENT QUAL

%nonassoc NOELSE 
%nonassoc ELSE 
%left EQ NEQ LT GT LEQ GEQ PLUS MINUS TIMES DIVIDE AND OR

%start main
%type <Ast.yagl_program> main

%%

main:
 | p = yagl_program; EOF { p } 

yagl_program:
(* This tuple represents the AST for the entire yagl program. 
   The first list is the variable declaration list, the second is the function declaration list 
   and the last one is top level expressions list *) 
 | (*no code *)                            { ([], [], []) }
 | p = yagl_program; v_dec = variable_defin    { (v_dec :: (first_elem p), (second_elem p), (third_elem p)) }
 | p = yagl_program; top_code = statement      { (first_elem p, second_elem p, top_code :: third_elem p) }
 | p = yagl_program; func_dec = function_defin { (first_elem p, (func_dec :: second_elem p), third_elem p)}

variable_defin:
 | q = QUAL; id = IDENT; ASSN_EQUAL; e = expression { {qual=q;
						       ident=id;
						       rhs=e} }
variable_defin_list:
 | (* No defined variables *)  { [] } 
 | l = variable_defin_list; v = variable_defin { v :: l }

function_defin: (* Possibly some issues here *)
 | FUNC; i = IDENT; LEFT_PAREN; ps = formals_opts; RIGHT_PAREN; locals = variable_defin_list; body = statement_list {{fname=i;
								 					              formals=ps;
								 						      locals=List.rev locals;
								  						      body=List.rev body}}
formals_opts:
 | (* No arguments needed for this function *) { [] } 
 | l = formal_list { List.rev l } 

formal_list:
 | q = QUAL; i = IDENT { [(q, i)] }
 | l = formal_list; COMMA; q = QUAL; i = IDENT { (q, i) :: l } 
				  
expression:
 | i = INTLITERAL { Literal(i) }
 | i = IDENT      { Id(i) }
 | e1 = expression; PLUS; e2 = expression  { BinOp(e1, Add, e2) }
 | e1 = expression; MINUS; e2 = expression { BinOp(e1, Sub, e2) }
 | e1 = expression; TIMES; e2 = expression { BinOp(e1, Mult, e2) }
 | e1 = expression; DIVIDE; e2 = expression { BinOp(e1, Div, e2) }
 | e1 = expression; EQ; e2 = expression { BinOp(e1, Equal, e2) }
 | e1 = expression; NEQ; e2 = expression { BinOp(e1, Neq, e2) }
 | e1 = expression; LT; e2 = expression { BinOp(e1, LessThan, e2) }
 | e1 = expression; LEQ; e2 = expression { BinOp(e1, Leq, e2) }
 | e1 = expression; GT; e2 = expression { BinOp(e1, GreaterThan, e2) }
 | e1 = expression; GEQ; e2 = expression { BinOp(e1, Geq, e2) }
 | e1 = expression; AND; e2 = expression { BinOp(e1, LogicalAnd, e2) }
 | e1 = expression; OR; e2 = expression { BinOp(e1, LogicalOr, e2) }
 | id = IDENT; LEFT_BRACKET; e = expression; RIGHT_BRACKET { ArrayIndex(id, e) } 

statement_list:
 | (* Empty list of statements *)    { [] }
 | l = statement_list; s = statement { s :: l }

statement:
 | e = expression { Expr(e) }
 | LEFT_BRACE; l = statement_list; RIGHT_BRACE { Block(List.rev l) } 
 | WHILE; LEFT_PAREN; e = expression; RIGHT_PAREN; l = statement; { While(e, l) }
 | RETURN; e = expression; NEWLINE { Return(e) }
 | BREAK                           { Break } 
 | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s = statement; %prec NOELSE                { If(e, s, Block([])) }
 | IF; LEFT_PAREN; e = expression; RIGHT_PAREN; s1 = statement; ELSE; s2 = statement { If(e, s1, s2) }
 | FOR; q = QUAL; id = IDENT; IN; d = IDENT; l = statement                           { For(q, id, d, l) }

