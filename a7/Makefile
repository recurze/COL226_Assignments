all: lexer.cmo parser.cmo solve.cmo main.cmo
	@(ocamlc -w -1..61 -g -o main unix.cma solve.cmo lexer.cmo parser.cmo main.cmo)

lexer.cmo: lexer.ml parser.cmi
	@(ocamlc -w -1..61 -c lexer.ml)

parser.cmo: parser.ml parser.cmi
	@(ocamlc -w -1..61 -c parser.ml)

parser.cmi: parser.mli
	@(ocamlc -w -1..61 -c parser.mli)

parser.mli parser.ml: parser.mly solve.cmo
	@(ocamlyacc parser.mly)

solve.cmo: solve.ml
	@(ocamlc -w -1..61 -c solve.ml)

main.cmo: main.ml
	@(ocamlc -w -1..61 -c main.ml)

lexer.ml: lexer.mll
	@(ocamllex lexer.mll)

clean:
	@(rm *.cmi *.cmo lexer.ml parser.ml parser.mli main)
