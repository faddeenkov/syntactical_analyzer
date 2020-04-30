open FuncDatatype
open FuncVar
open Cil
open JsonParser

(* Default output if the input-query is not supported *)
let loc_default = {line = -1; file = ""; byte = -1}

(* Naming of functions: resolve_query_[kind]_[find]_[structure] *)

(* Resolution of datatype-oriented queries *)
let resolve_query_datatype_decl_none query cilfile = 
match query.tar with Name_t(name) -> (find_decl name cilfile)::[]
                | AllGlobVar_t -> find_decl_all_glob cilfile
                | All_t -> find_decl_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_fun query cilfile funname = 
match query.tar with Name_t(name) -> FuncDatatype.find_uses_in_fun name funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_none query cilfile =
match query.tar with Name_t(name) -> FuncDatatype.find_uses name cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses query cilfile =
match query.str with Fun_s(funname) -> resolve_query_datatype_uses_fun query cilfile funname
                | None_s -> resolve_query_datatype_uses_none query cilfile
                | _ ->  Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype query cilfile = 
match query.f with Decl_f -> if ((query.str = None_s)) then (resolve_query_datatype_decl_none query cilfile) else (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[])
                | Uses_f -> resolve_query_datatype_uses query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

(* Resolution of variable-oriented queries *)
let resolve_query_var_uses_fun query cilfile funname = 
match query.tar with Name_t(name) -> FuncVar.find_uses_in_fun name (-1) funname cilfile
                | ID_t(id) -> FuncVar.find_uses_in_fun "" id funname cilfile
                | AllGlobVar_t -> FuncVar.find_uses_in_fun_all_glob funname cilfile
                | All_t -> FuncVar.find_uses_in_fun_all funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_uses_none query cilfile =
match query.tar with Name_t(name) -> FuncVar.find_uses name (-1) cilfile
                | ID_t(id) -> FuncVar.find_uses "" id cilfile
                | AllGlobVar_t -> FuncVar.find_uses_all_glob cilfile
                | All_t -> FuncVar.find_uses_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_uses_cond query cilfile =
match query.tar with Name_t(name) -> FuncVar.find_uses_in_cond name (-1) cilfile
                | ID_t(id) -> FuncVar.find_uses_in_cond "" id cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_uses query cilfile = 
match query.str with Fun_s(funname) -> resolve_query_var_uses_fun query cilfile funname
                | Cond_s -> resolve_query_var_uses_cond query cilfile
                | None_s -> resolve_query_var_uses_none query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var query cilfile = 
match query.f with Uses_f -> resolve_query_var_uses query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

(* Main mapping function *)
let map_query query cilfile = if (query.lim != None_c) then (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]) else (
match query.k with Datatype_k -> resolve_query_datatype query cilfile
                | Var_k -> resolve_query_var query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[] )