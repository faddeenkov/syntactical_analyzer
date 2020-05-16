# Syntactical analyzer
This project is still in progress.

## Installation


```bash
apt install ocaml
add-apt-repository ppa:avsm/ppa
apt update
apt install opam
opam init
opam switch create . ocaml-base-compiler.4.09.0
eval $(opam env)
```
Install the following opam-packages: dune, zarith, ounit,  yojson and ppx_deriving_yojson with 
```bash
opam install [package]
```
For some of these packages (for example zarith) you will need to install m4 and libgmp-dev with:
```bash
apt install [package]
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
The executable takes two arguments: path of the JSON-file and path of the C-file.

## Query Language
The following JSON-grammar describes the current (more or less supported) structure of a query:
```bash
{
"select" : "$ALL",
"type" : "var" | "fun" | "datatype",
"target" : {"name" : S} | {"id" : I} | "$ALL" | "$ALL_GLOB_VAR" | {"and" : [S, S, ...]} | {"or" : [S, ...]}, # where S is a string and I is an integer
"find" : "uses" | "decl" | "defs" | {"uses_with_var" : S} | "returns",
"structure" : {"fun_name" : S} | "cond" | "non-cond" | "$NONE"
}
```
Note that the use of some parameters is restricted to some types.
