
OCAMLCC=ocamlc
OSRC=utils.ml ast.ml dot.ml solver.ml typesystem.ml dags.ml compiler.ml
OSRCL=utils.mli ast.mli dot.mli typesystem.mli

.PHONY: test clean doc

doc/index.html: $(OSRCL) $(OSRC)
	ocamldoc -html -d doc $(OSRCL)

doc: doc/index.html
	open doc/index.html

tests: $(OSRC) $(OSRCL) tests.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -g -o tests $(OSRC) tests.ml
	./tests


examples: $(OSRC) $(OSRCL) examples.ml
	$(OCAMLCC) $(OSRCL)
	$(OCAMLCC) -g -o examples $(OSRC) examples.ml
	./examples
	dot -Tpdf example1.dot > example1.pdf && open example1.pdf	
	dot -Tpdf example2.dot > example2.pdf && open example2.pdf	
	dot -Tpdf example3.dot > example3.pdf && open example3.pdf	
	dot -Tpdf example4.dot > example4.pdf && open example4.pdf	
	dot -Tpdf example5.dot > example5.pdf && open example5.pdf	
	dot -Tpdf example6.dot > example6.pdf && open example6.pdf	
	dot -Tpdf example7.dot > example7.pdf && open example7.pdf	

clean:
	rm *.cmi
	rm *.cmo
	rm *.html
	rm *.css
