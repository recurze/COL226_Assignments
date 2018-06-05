all: lexer.cmo parser.cmo calc.cmo typefile.cmo
	ocamlc -o calc typefile.cmo lexer.cmo parser.cmo calc.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -c lexer.ml

parser.cmo: parser.ml parser.cmi
	ocamlc -c parser.ml

calc.cmo: calc.ml
	ocamlc -c calc.ml

parser.cmi: parser.mli
	ocamlc -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll       # generates lexer.ml

parser.mli: parser.mly typefile.cmo
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

parser.ml: parser.mly
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

typefile.cmo: typefile.ml
	ocamlc -c typefile.ml

clean:
	rm -f *.cm*
	rm -f *.mli
	rm -f lexer.ml parser.ml

submit:
	rm -rf 2015CS10262*
	mkdir 2015CS10262_Assignment3
	cp parser.mly lexer.mll calc.ml typefile.ml README.md Makefile calc ./2015CS10262_Assignment3/
	zip -r 2015CS10262_Assignment3 2015CS10262_Assignment3
