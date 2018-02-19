all:
	ocamlfind ocamlopt -o P -linkpkg -package extlib Errors.ml Result.ml Stream.ml Types.ml Combinators.ml Tests.ml
