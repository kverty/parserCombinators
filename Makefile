all:
	ocamlfind ocamlopt -o P -linkpkg -package extlib Errors.ml Types.ml Combinators.ml Tests.ml
