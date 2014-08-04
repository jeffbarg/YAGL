OBJS = ast.cmo parser.cmo scanner.cmo yaglc.cmo

yaglc: $(OBJS)
	ocamlc -o yaglc $(OBJS)

scanner.ml:scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli:parser.mly
	menhir parser.mly

%.cmo:%.ml
	ocamlc -c $<

%.cmi:%.mli
	ocamlc -c $<

.PHONY:clean 

clean:
	rm -rf scanner.ml *.mli .*~ *.cmo *.cmi *.out 
	@echo "Finished cleaning up files"


