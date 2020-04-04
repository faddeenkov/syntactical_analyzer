-how to compile: e.g. a file myMain.ml using CIL-libraries
ocamlopt -c -I cil/lib/cil/ myMain.ml
ocamlfind ocamlopt -ccopt -Lcil/lib/cil -o main unix.cmxa str.cmxa cil/lib/cil/cil.cmxa -package zarith -linkpkg myMain.cmx

ocamlfind ocamlopt -o main3  src/jsonParser.ml -package yojson -linkpkg


TODO:
- schauen, dass das Kompilieren von Yojson und Cil klappt (Modul wird schon mal gefunden, Implementierung nicht?!)
- schauen, wie ich meine main.ml und [module].ml Struktur kompilieren kann
- Tests erstellen
- toString von meiner geparsten Struktur erstellen
- Exceptions einführen (besser als das handlen durch Default-parameter)
