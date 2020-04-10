FILENAME = jsonParser

.PHONY: clean
clean:
	rm -r -f src/_build
	rm -f src/.merlin
	rm -r -f test/_build
	rm -f test/.merlin
	rm -f prog
	rm -f src/jsonParser.cmi
	rm -f src/jsonParser.cmx
	rm -f src/jsonParser.o
	rm -f src/main.cmi
	rm -f src/main.cmx
	rm -f src/main.o
	rm -f 'oUnit-test suite for JSONParser.cache'
	rm -f 'oUnit-test suite for JSONParser-olga-VirtualBox#00.log'
	rm -f 'oUnit-test suite for JSONParser-olga-VirtualBox#01.log'
	rm -f 'oUnit-test suite for JSONParser-olga-VirtualBox#02.log'
	rm -f test/jsonParser_test.cmi
	rm -f test/jsonParser_test.cmx
	rm -f test/jsonParser_test.o	

compile_dune: 
	cd src; echo "I'm in src"; \
        dune build main.exe 
#dune build --only-packages=src --build-dir=build main.exe

compile_ocamlfind:
	ocamlfind ocamlopt -I src -o prog -linkpkg -package yojson src/jsonParser.ml src/main.ml

test_dune:
	cd test; echo "I'm in test"; \
		dune build jsonParser_test.exe

test_ocamlfind:
	ocamlfind ocamlopt -I src -o prog -linkpkg -package yojson -package ounit2 src/jsonParser.ml test/jsonParser_test.ml
