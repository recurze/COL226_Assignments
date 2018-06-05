all: lexer.cmo parser.cmo prolog.cmo structure.cmo
	ocamlc -g -o prolog unix.cma structure.cmo lexer.cmo parser.cmo prolog.cmo

lexer.cmo: lexer.ml parser.cmi
	ocamlc -g -c lexer.ml

parser.cmo: parser.ml parser.cmi
	ocamlc -g -c parser.ml

prolog.cmo: prolog.ml
	ocamlc -g -c prolog.ml

parser.cmi: parser.mli
	ocamlc -g -c parser.mli

lexer.ml: lexer.mll
	ocamllex lexer.mll       # generates lexer.ml

parser.mli: parser.mly structure.cmo
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

parser.ml: parser.mly
	ocamlyacc parser.mly     # generates parser.ml and parser.mli

structure.cmo: structure.ml
	ocamlc -g -c structure.ml

clean:
	rm -f *.cm*
	rm -f *.mli
	rm -f lexer.ml parser.ml

submit:
	rm -rf 2015CS10262*
	mkdir 2015CS10262_Assignment6
	cp parser.mly lexer.mll *.ml *.pl README.md Makefile prolog ./2015CS10262_Assignment6/
	zip -r 2015CS10262_Assignment6 2015CS10262_Assignment6
