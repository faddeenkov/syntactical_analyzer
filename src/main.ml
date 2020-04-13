open JsonParser
open FuncDatatype

(* Reads input *)
let filename = Sys.argv.(1)

let _ =
let query = parse_json_file filename 
in Printf.printf "%s" (to_string query); f filename (* Second statement does not make any sense, just testing dependencies *)
