all: interpreter_secd.ml lexer.mll parser.mly
	ocamlc -c -g types.ml
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlc -c -g parser.mli
	ocamlc -c -g lexer.ml
	ocamlc -c -g parser.ml
	ocamlc -c -g interpreter_secd.ml
	ocamlc -g -o secd types.cmo lexer.cmo parser.cmo interpreter_secd.cmo

clean:
	rm -rf ./*.cmo
	rm -rf ./*.cmi
	rm -rf ./lexer.ml
	rm -rf ./parser.ml
	rm -rf ./*.mli