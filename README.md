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
Install following opam-packages: dune, zarith, ounit,  yojson and ppx_deriving_yojson with 
```bash
opam install [package]
```
For some of these packages (for example zarith) you will need to install m4 and libgmp-dev with:
```bash
sudo apt install [package]
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
