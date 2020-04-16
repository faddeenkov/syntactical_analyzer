open FuncDatatype
open Cil
open JsonParser

let loc_default = {line = -1; file = ""; byte = -1}

let resolve_query_datatype_none query cilfile = 
match query.tar with Name_t(name) -> (find_decl name cilfile)::[]
                | AllGlobVar_t -> find_decl_all_glob cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype query cilfile = 
match query.f with Decl_f -> if ((query.str = None_s) && (query.lim = None_c)) then (resolve_query_datatype_none query cilfile) else (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[])
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let map_query query cilfile = 
match query.k with Datatype_k -> resolve_query_datatype query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]