# Syntactical analyzer


## Installation


```bash
apt install ocaml
add-apt-repository ppa:avsm/ppa
apt update
apt install opam
opam init
eval %(opam env)
opam switch create . ocaml-base-compiler.4.09.0
eval %(opam env)
opam install dune
opam install zarith
```

## Usage

Theoretically you can build it using
```bash
dune build
```
and test it with 
```bash
dune test
```
