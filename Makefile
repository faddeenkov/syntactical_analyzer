FILENAME = jsonParser

.PHONY: clean
clean:
	rm $(addprefix src/$(FILENAME),.cmi .cmx .o )
	rm main

compile: src/jsonParser.ml
	ocamlfind ocamlopt -I cil/lib/cil -o main src/jsonParser.ml -package yojson -package zarith -linkpkg