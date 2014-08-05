clean:
	rm -rf scanner.ml parser.ml *.mli *.*~* *.cmo *.cmi *.out *.c ./_build *.native
	@echo "Finished cleaning up files"

install:
	ocamlbuild -use-menhir yaglc.native
