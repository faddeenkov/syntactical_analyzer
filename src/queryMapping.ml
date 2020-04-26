open FuncDatatype
open FuncVar
open Cil
open JsonParser

(* Default output if the input-query is not supported *)
let loc_default = {line = -1; file = ""; byte = -1}

(* Naming of functions: resolve_query_[kind]_[find]_[structure]_[constraint] *)

(* Resolution of datatype-oriented queries *)
let resolve_query_datatype_decl_none query cilfile = 
match query.tar with Name_t(name) -> (find_decl name cilfile)::[]
                | AllGlobVar_t -> find_decl_all_glob cilfile
                | All_t -> find_decl_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_fun_none query cilfile funname = 
match query.tar with Name_t(name) -> FuncDatatype.find_uses_in_fun name funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses query cilfile =
match query.str with Fun_s(funname) -> if(query.lim = None_c) then resolve_query_datatype_uses_fun_none query cilfile funname else (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[])
                | _ ->  Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype query cilfile = 
match query.f with Decl_f -> if ((query.str = None_s) && (query.lim = None_c)) then (resolve_query_datatype_decl_none query cilfile) else (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[])
                | Uses_f -> resolve_query_datatype_uses query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

(* Resolution of variable-oriented queries *)
let resolve_query_var_uses_fun_none query cilfile funname = 
match query.tar with Name_t(name) -> FuncVar.find_uses_in_fun name (-1) funname cilfile
                | ID_t(id) -> FuncVar.find_uses_in_fun "" id funname cilfile
                | AllGlobVar_t -> FuncVar.find_uses_in_fun_all_glob funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_uses query cilfile = 
match query.str with Fun_s(funname) -> if(query.lim = None_c) then resolve_query_var_uses_fun_none query cilfile funname else (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[])
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var query cilfile = 
match query.f with Uses_f -> resolve_query_var_uses query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

(* Main mapping function *)
let map_query query cilfile = 
match query.k with Datatype_k -> resolve_query_datatype query cilfile
                | Var_k -> resolve_query_var query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]