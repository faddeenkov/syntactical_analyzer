FILENAME = jsonParser

.PHONY: clean
clean:
	rm -r -f src/_build
	rm -f src/.merlin
	rm -r -f test/_build
	rm -f test/.merlin
	rm -f prog
	rm -f src/*.cm*
	rm -f src/*.o
	rm -f test/*.cm*
	rm -f test/*.o
	rm -f *.cm*
	rm -f *.o
	rm -f 'oUnit-test suite for JSONParser.cache'
	rm -f 'oUnit-test suite for JSONParser-olga-VirtualBox#00.log'
	rm -f 'oUnit-test suite for JSONParser-olga-VirtualBox#01.log'
	rm -f 'oUnit-test suite for JSONParser-olga-VirtualBox#02.log'

compile_ocamlfind:
	ocamlfind ocamlopt -I src -o prog -linkpkg -package yojson src/jsonParser.ml src/main.ml

test_ocamlfind:
	ocamlfind ocamlopt -I src -o prog -linkpkg -package yojson -package ounit2 src/jsonParser.ml test/jsonParser_test.ml

	#compile with ppx_deriving_yojson: ocamlfind ocamlopt -package yojson -package ppx_deriving_yojson src/yojsonDeriver.ml
