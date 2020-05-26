open Mylib.JsonParser
open Mylib.QueryMapping
open Mylib.ResultPrinter
open Cil

let jsonFile = Sys.argv.(1)
let sourceFile = Sys.argv.(2)

(* This actually parses and executes the query *)
let executeQuery () = 
let query = parse_json_file jsonFile
in let result = map_query (query) (Frontc.parse sourceFile ())
in Printf.printf "%s" (print_result result query)

(* Replace function name with jsonDeriverTest find_uses_in_funTest or executeQuery *)
let _ = executeQuery ()