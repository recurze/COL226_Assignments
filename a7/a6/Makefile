SRC=exceptions.ml types.ml helper_functions.ml mgu.ml

OBJ=$(SRC:.ml=.cmo)

%.cmo: %.ml
	ocamlc -c $@ $<

main: ${OBJ}
	ocamlc -o $@ $^

clean:
	rm *.cmo *.cmi main
