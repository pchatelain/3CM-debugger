OCC = ocamlc
OCC_FLAGS = -c -g
OCL_FLAGS = -g
OCAMLDEP = ocamldep
SRC = ast.ml interval.ml analysis.ml lexer.ml parser.ml wellformed.ml eval.ml main.ml

compile: $(SRC:.ml=.cmo)
	$(OCC) $(OCL_FLAGS) -o 3cm $(SRC:.ml=.cmo)

windows: $(SRC:.ml=.cmo)
	$(OCC) $(OCL_FLAGS) -o 3cm.exe $(SRC:.ml=.cmo)

toplevel:  $(SRC:.ml=.cmo)
	ocamlmktop -o 3cmocaml $(SRC:.ml=.cmo)

lexer.ml: lexer.mll
	ocamllex lexer.mll


parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.ml

parser.cmo: parser.cmi

lexer.cmo: parser.cmo

%.cmi: %.mli
	$(OCC) $(OCC_FLAGS) $<

%.cmo: %.ml
	$(OCC) $(OCC_FLAGS) $<

clean:
	rm -f parser.ml parser.mli lexer.ml *.cmi *.cmo *.output *~

# Dependencies
depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

include .depend
