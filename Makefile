FILENAME = jsonParser

.PHONY: clean
clean_old:
	rm $(addprefix src/$(FILENAME),.cmi .cmx .o )
	rm main

clean:
	rm -r src/_build
	rm src/.merlin

compile_old: src/jsonParser.ml
	ocamlfind ocamlopt -I cil/lib/cil -o main src/jsonParser.ml src/main.ml -package yojson -package zarith -linkpkg

compile: 
	cd src; echo "I'm in src"; \
        dune build main.exe 
#dune build --only-packages=src --build-dir=build main.exe