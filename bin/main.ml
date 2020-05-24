open Mylib.JsonParser
open Mylib.QueryMapping
open Mylib.FuncVar
open Mylib.FuncDatatype
open Mylib.ResultPrinter
open Yojson.Safe
open Cil

let jsonFile = Sys.argv.(1)
let sourceFile = Sys.argv.(2)

exception Error of string
(*
let rec print_result list =
match list with (name, loc, kind, id)::xs -> Printf.printf "name:%s, loc.line=%d, loc.file=%s, loc.byte:%d, kind:%s, id:%d \n" name loc.line loc.file loc.byte kind id; print_result xs
            | [] -> () *)

let find_type_uses_in_funTest () = print_result (Mylib.FuncDatatype.find_uses_in_fun ("int") ("main") (Frontc.parse sourceFile ()))

(* This actually parses and executes the query *)
let executeQuery () = 
let query = parse_json_file jsonFile
in let result = map_query (query) (Frontc.parse sourceFile ())
in Printf.printf "%s" (print_result result query)

(* This tests to_yojson *)
let testToJson () =
let query = {sel = [Name_sel]; 
            k = Fun_k;
            tar = All_t;
            f = UsesWithVar_f("x");
            str = Fun_s("a function");
            lim = None_c}
in Printf.printf "%s\n" (Yojson.Safe.to_string (query_to_yojson query))

(* Replace function name with jsonDeriverTest find_uses_in_funTest or executeQuery *)
let _ = executeQuery ()