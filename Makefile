
OCAMLCC=ocamlc
OSRC=utils.ml ast.ml dot.ml typesystem.ml compiler.ml 
OSRCL=utils.mli ast.mli dot.mli

.PHONY: test clean doc

comp: $(OSRC) $(OSRCL)
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -o comp $(OSRC)

test.pdf: comp
	./comp
	dot -Tpdf output.dot > test.pdf

doc/index.html: $(OSRCL) $(OSRC)
	ocamldoc -html -d doc $(OSRCL)

doc: doc/index.html


test: test.pdf
	open test.pdf

clean:
	rm *.cmi
	rm *.cmo
	rm *.html
	rm *.css
