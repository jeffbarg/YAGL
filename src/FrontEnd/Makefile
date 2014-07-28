scanner.ml:scanner.mll
	ocamllex scanner.mll

terminals.cmo:terminals.ml
	ocamlc -c terminals.ml

all: scanner.ml terminals.cmo
	@echo "Built everything"
