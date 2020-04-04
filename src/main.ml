open JSONParser

(* Reads input *)
let filename = Sys.argv.(1)

let _ = parse_json_file filename