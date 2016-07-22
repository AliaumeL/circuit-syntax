
OCAMLCC=ocamlc
OSRC=utils.ml lexer.ml ast.ml parser.ml dot.ml solver.ml typesystem.ml dags.ml compiler.ml ptg.ml rewriting.ml
OSRCL=utils.mli ast.mli dot.mli typesystem.mli dags.mli

.PHONY: test clean doc

doc/index.html: $(OSRCL) $(OSRC)
	ocamldoc -html -d doc $(OSRCL)

doc: doc/index.html
	open doc/index.html

tests: $(OSRC) $(OSRCL) tests.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -g -o tests $(OSRC) tests.ml
	./tests

circuits: $(OSRC) $(OSRCL) circuits.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -g -o circuits $(OSRC) circuits.ml
	./circuits

clean:
	rm *.cmi
	rm *.cmo
