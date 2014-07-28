scanner.ml:scanner.mll parser.ml
	ocamllex scanner.mll

parser.ml: parser.mly ast.cmo
	menhir parser.mly

ast.cmo:ast.ml
	ocamlc -c ast.mli

# if you just do make, then only the first item in the file
# will be done, other wise do make all to set off this all 
# declaration and its dependencies, splitting lines requires "\"
# note that all env variables are also visible to make, so 
# you can refer to them directly with $PATH and whatnot. 
all: scanner.ml 
	@echo "Built everything for the front end" 

output_test.cmo:output_test.ml
	ocamlc -c output_test.ml

test.native:output_test.cmo scanner.cmo parser.cmo
	ocamlc -o test.native parser.cmo scanner.cmo output_test.cmo

test: test.native
	@echo example.yagl | test.native

