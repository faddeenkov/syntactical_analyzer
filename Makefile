FILENAME = jsonParser

.PHONY: clean
clean:
	rm -r -f src/_build
	rm -f src/.merlin
	rm -r -f test/_build
	rm -f test/.merlin

compile: 
	cd src; echo "I'm in src"; \
        dune build main.exe 
#dune build --only-packages=src --build-dir=build main.exe

tests:
	cd test; echo "I'm in test"; \
		dune build jsonParser_test.exe