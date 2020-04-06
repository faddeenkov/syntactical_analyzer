-how to compile: e.g. a file myMain.ml using CIL-libraries
ocamlopt -c -I cil/lib/cil/ myMain.ml
ocamlfind ocamlopt -ccopt -Lcil/lib/cil -o main unix.cmxa str.cmxa cil/lib/cil/cil.cmxa -package zarith -linkpkg myMain.cmx

ocamlfind ocamlopt -o main3  src/jsonParser.ml -package yojson -linkpkg


