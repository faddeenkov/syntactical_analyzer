open Mylib.JsonParser
open Mylib.QueryMapping
open Mylib.FuncVar
open Yojson.Safe
open Cil

let jsonFile = Sys.argv.(1)
let sourceFile = Sys.argv.(2)

exception Error of string

let rec print_result list =
match list with (name, loc, kind, id)::xs -> Printf.printf "name:%s, loc.line=%d, loc.file=%s, loc.byte:%d, kind:%s, id:%d \n" name loc.line loc.file loc.byte kind id; print_result xs
            | [] -> ()

let find_uses_in_funTest () = print_result (find_uses_in_fun ("x2") (-1) ("main") (Frontc.parse sourceFile ()))

(* This actually parses and executes the query *)
let executeQuery () = 
let result = map_query (parse_json_file jsonFile) (Frontc.parse sourceFile ())
in
let rec print_result list =
match list with (name, loc, kind, id)::xs -> Printf.printf "name:%s, loc.line=%d, loc.file=%s, loc.byte:%d, kind:%s, id:%d \n" name loc.line loc.file loc.byte kind id; print_result xs
            | [] -> ()
in print_result result 

(* This tests to_yojson *)
let testToJson () =
let query = {sel = [Name_sel; Location_sel]; 
            k = Var_k;
            tar = Or_t(["x";"y"]);
            f = Decl_f;
            str = None_s;
            lim = Constraint_c("x==y")}
in Printf.printf "%s\n" (Yojson.Safe.to_string (query_to_yojson query))
(* Replace function name with jsonDeriverTest find_uses_in_funTest or executeQuery *)
let _ = executeQuery ()