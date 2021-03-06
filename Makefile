
OCAMLCC=ocamlc
OSRC=utils.ml lexer.ml ast.ml parser.ml dot.ml solver.ml typesystem.ml dags.ml compiler.ml ptg.ml rewriting.ml

.PHONY: test clean doc

doc/index.html: $(OSRC)
	ocamldoc -html -d doc $(OSRCL)

doc: doc/index.html
	open doc/index.html

tests: $(OSRC) tests.ml
	$(OCAMLCC) -g -o tests $(OSRC) tests.ml
	./tests

circuits: $(OSRC) circuits.ml
	mkdir -p graphics
	$(OCAMLCC) -g -o circuits $(OSRC) circuits.ml

clean:
	rm *.cmi
	rm *.cmo
	rm graphics/*.pdf
	rm graphics/*.dot
