open Mylib.JsonParser
open Mylib.QueryMapping
open Mylib.FuncVar
open Yojson.Safe
open Cil

let jsonFile = Sys.argv.(1)
let sourceFile = Sys.argv.(2)

let jsonDeriverTest () = Printf.printf "main started\n";
let query = Printf.printf "now query will be parsed\n"; parse_json_file jsonFile 
in let derived = Printf.printf "query_of_yojson will be called\n"; query_of_yojson (from_file jsonFile)  (* Second statement does not make any sense, just testing dependencies *)
in Printf.printf "matching time!\n"; match derived with Result.Ok y -> Printf.printf "No error occured\n"; Mylib.JsonParser.to_string y
                    | Result.Error x -> Printf.printf "An error occured: %s\n" x; "" 

let rec print_result list =
match list with (name, loc, kind, id)::xs -> Printf.printf "name:%s, loc.line=%d, loc.file=%s, loc.byte:%d, kind:%s, id:%d \n" name loc.line loc.file loc.byte kind id; print_result xs
            | [] -> ()

let find_uses_in_funTest () = print_result (find_uses_in_fun ("x2") ("main") (Frontc.parse sourceFile ()))

(* This test actually parses and executes the query *)
let mapTest () = let query = parse_json_file jsonFile
in 
let result = map_query query (Frontc.parse sourceFile ())
in
let rec print_result list =
match list with (name, loc, kind, id)::xs -> Printf.printf "name:%s, loc.line=%d, loc.file=%s, loc.byte:%d, kind:%s, id:%d \n" name loc.line loc.file loc.byte kind id; print_result xs
            | [] -> ()
in print_result result

(* Replace function name with jsonDeriverTest find_uses_in_funTest or mapTest *)
let _ = find_uses_in_funTest ()