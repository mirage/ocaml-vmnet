vmnet.cmx: vmnet.ml vmnet.mli vmnet_stubs.c
	ocamlfind ocamlopt -verbose -ccopt '-Wall -g' -cclib '-framework vmnet' -package bigarray,unix,cstruct.unix,cstruct.syntax,sexplib,sexplib.syntax -syntax camlp4o -linkpkg vmnet_stubs.c vmnet.mli vmnet.ml
