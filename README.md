# Syntactical analyzer


## Installation


```bash
apt install ocaml
add-apt-repository ppa:avsm/ppa
apt update
apt install opam
opam init
opam switch create . ocaml-base-compiler.4.09.0
eval %(opam env)
```
Install following opam-packages: dune, zarith, yojson and ppx_deriving_yojson with 
```bash
opam install [package]
```

## Usage

You can build it using
```bash
dune build
```
(or with ignoring warnings):
```bash
dune build --profile release
```