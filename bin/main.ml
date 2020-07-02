open Mylib.JsonParser
open Mylib.QueryMapping
open Mylib.ResultPrinter
open Cil
open Cabs2cil

let jsonFile = Sys.argv.(1)
let sourceFile = Sys.argv.(2)

(* This actually parses and executes the query *)
let executeQuery () = 
let query = parse_json_file jsonFile
in let result = map_query (query) (Frontc.parse sourceFile ())
in Printf.printf "%s" (print_result result query)

let print_varnameMapping () =
ignore(map_query (parse_json_file jsonFile) (Frontc.parse sourceFile ())); Hashtbl.iter (fun a b -> Printf.printf "%s is mapped to %s\n" a b) varnameMapping

let _ = executeQuery ()