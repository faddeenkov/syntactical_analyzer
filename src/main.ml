open JsonParser

(* Reads input *)
let filename = Sys.argv.(1)

let _ =
let query = parse_json_file filename
in Printf.printf "%s" (to_string query)
