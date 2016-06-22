
OCAMLCC=ocamlc
OSRC=utils.ml ast.ml dot.ml solver.ml typesystem.ml
OSRCL=utils.mli ast.mli dot.mli typesystem.mli

.PHONY: test clean doc

comp: $(OSRC) $(OSRCL) compiler.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -g -o comp $(OSRC) compiler.ml

test.pdf: comp
	./comp
	dot -Tpdf output.dot > test.pdf

doc/index.html: $(OSRCL) $(OSRC)
	ocamldoc -html -d doc $(OSRCL)

doc: doc/index.html


test: test.pdf
	open test.pdf

tests: $(OSRC) $(OSRCL) tests.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -g -o tests $(OSRC) tests.ml
	./tests


examples: $(OSRC) $(OSRCL) examples.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -o examples $(OSRC) compiler.ml examples.ml
	./examples

clean:
	rm *.cmi
	rm *.cmo
	rm *.html
	rm *.css
