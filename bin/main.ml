open Mylib.JsonParser
open Mylib.QueryMapping
open Yojson.Safe
open Cil

(* Reads input *)
let jsonFile = Sys.argv.(1)

let sourceFile = Sys.argv.(2)

let _ = let query = parse_json_file jsonFile
in (* Printf.printf "%s" (Mylib.JsonParser.to_string query); *)match (map_query query (Frontc.parse sourceFile ())) with (name, loc, kind, id) -> Printf.printf "name: %s, loc.line= %d, loc.file= %s, loc.byte: %d, kind: %s, id: %d \n" name loc.line loc.file loc.byte kind id  

(* let _ = Printf.printf "main started\n";
let query = Printf.printf "now query will be parsed\n"; parse_json_file jsonFile 
in let derived = Printf.printf "query_of_yojson will be called\n"; query_of_yojson (from_file jsonFile)  (* Second statement does not make any sense, just testing dependencies *)
in Printf.printf "matching time!\n"; match derived with Result.Ok y -> Printf.printf "No error occured\n"; Mylib.JsonParser.to_string y
                    | Result.Error x -> Printf.printf "An error occured: %s\n" x; "" *)

(* Printf.printf "%s" (Mylib.JsonParser.to_string query); let _ =  f jsonFile *)