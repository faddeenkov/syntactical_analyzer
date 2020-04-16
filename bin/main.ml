open Mylib.JsonParser
open Mylib.FuncDatatype
open Yojson.Safe

(* Reads input *)
let filename = Sys.argv.(1)

let _ = let query = parse_json_file filename
in Printf.printf "%s" (Mylib.JsonParser.to_string query)

(* let _ = Printf.printf "main started\n";
let query = Printf.printf "now query will be parsed\n"; parse_json_file filename 
in let derived = Printf.printf "query_of_yojson will be called\n"; query_of_yojson (from_file filename)  (* Second statement does not make any sense, just testing dependencies *)
in Printf.printf "matching time!\n"; match derived with Result.Ok y -> Printf.printf "No error occured\n"; Mylib.JsonParser.to_string y
                    | Result.Error x -> Printf.printf "An error occured: %s\n" x; "" *)

(* Printf.printf "%s" (Mylib.JsonParser.to_string query); let _ =  f filename *)