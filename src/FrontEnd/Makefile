scanner.ml:scanner.mll
	ocamllex scanner.mll

terminals.cmo:terminals.ml
	ocamlc -c terminals.ml
# if you just do make, then only the first item in the file
# will be done, other wise do make all to set off this all 
# declaration and its dependencies, splitting lines requires "\"
# note that all env variables are also visible to make, so 
# you can refer to them directly with $PATH and whatnot. 
all: scanner.ml terminals.cmo
	@echo "Built everything"

