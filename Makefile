all:
	ocamlfind ocamlopt -o P -linkpkg -rectypes -package extlib -package re Errors.ml Result.ml Stream.ml Types.ml Combinators.ml Tests.ml
