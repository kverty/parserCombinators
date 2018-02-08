all:
	ocamlfind ocamlopt -o P -linkpkg -package extlib Combinators.ml Tests.ml
