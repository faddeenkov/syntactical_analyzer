open FuncDatatype
open FuncVar
open FuncFunction
open Cil
open JsonParser

(* Default output if the input-query is not supported *)
let loc_default = {line = -1; file = ""; byte = -1}

(* Naming of functions: resolve_query_[kind]_[find]_[structure] *)

(* Resolution of datatype-oriented queries *)
let resolve_query_datatype_decl_none query cilfile = 
match query.tar with Name_t(name) -> FuncDatatype.find_decl name cilfile
                | AllGlobVar_t -> FuncDatatype.find_decl_all_glob cilfile
                | All_t -> FuncDatatype.find_decl_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_fun query cilfile funname = 
match query.tar with Name_t(name) -> FuncDatatype.find_uses_in_fun name funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_none query cilfile =
match query.tar with Name_t(name) -> FuncDatatype.find_uses name cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_cond query cilfile =
match query.tar with Name_t(name) -> FuncDatatype.find_uses_in_cond name cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses_noncond query cilfile =
match query.tar with Name_t(name) -> FuncDatatype.find_uses_in_noncond name cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_uses query cilfile =
match query.str with Fun_s(funname) -> resolve_query_datatype_uses_fun query cilfile funname
                | Cond_s -> resolve_query_datatype_uses_cond query cilfile
                | NonCond_s -> resolve_query_datatype_uses_noncond query cilfile
                | None_s -> resolve_query_datatype_uses_none query cilfile

let resolve_query_datatype_defs_none query cilfile = 
match query.tar with Name_t(name) -> FuncDatatype.find_def name cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype_defs query cilfile =
match query.str with None_s -> resolve_query_datatype_defs_none query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_datatype query cilfile = 
match query.f with Decl_f -> if ((query.str = None_s)) then (resolve_query_datatype_decl_none query cilfile) else (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[])
                | Uses_f -> resolve_query_datatype_uses query cilfile
                | Defs_f -> resolve_query_datatype_defs query cilfile
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
                | AllGlobVar_t -> FuncVar.find_uses_in_cond_all_glob cilfile
                | All_t -> FuncVar.find_uses_in_cond_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_uses_noncond query cilfile =
match query.tar with Name_t(name) -> FuncVar.find_uses_in_noncond name (-1) cilfile
                | ID_t(id) -> FuncVar.find_uses_in_noncond "" id cilfile
                | AllGlobVar_t -> FuncVar.find_uses_in_noncond_all_glob cilfile
                | All_t -> FuncVar.find_uses_in_noncond_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_uses query cilfile = 
match query.str with Fun_s(funname) -> resolve_query_var_uses_fun query cilfile funname
                | Cond_s -> resolve_query_var_uses_cond query cilfile
                | NonCond_s -> resolve_query_var_uses_noncond query cilfile
                | None_s -> resolve_query_var_uses_none query cilfile

let resolve_query_var_decl_fun query cilfile funname =
match query.tar with Name_t(name) -> FuncVar.find_decl_in_fun name (-1) funname cilfile
                | ID_t(id) -> FuncVar.find_decl_in_fun "" id funname cilfile
                | All_t -> FuncVar.find_decl_in_fun_all funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_decl_none query cilfile =
match query.tar with AllGlobVar_t -> FuncVar.find_decl_all_glob cilfile
                | Name_t(name) -> FuncVar.find_decl name (-1) cilfile
                | ID_t (id) -> FuncVar.find_decl "" id cilfile
                | All_t -> FuncVar.find_decl_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_decl query cilfile =
match query.str with Fun_s(funname) -> resolve_query_var_decl_fun query cilfile funname
                | None_s -> resolve_query_var_decl_none query cilfile
                | NonCond_s -> resolve_query_var_decl_none query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_defs_fun query cilfile funname =
match query.tar with Name_t(name) -> FuncVar.find_defs_in_fun name (-1) funname cilfile
                | ID_t(id) -> FuncVar.find_defs_in_fun "" id funname cilfile
                | AllGlobVar_t -> FuncVar.find_defs_in_fun_all_glob funname cilfile
                | All_t -> FuncVar.find_defs_in_fun_all funname cilfile 
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_defs_none query cilfile =
match query.tar with Name_t(name) -> FuncVar.find_defs name (-1) cilfile
                | ID_t(id) -> FuncVar.find_defs "" id cilfile
                | AllGlobVar_t -> FuncVar.find_defs_all_glob cilfile
                | All_t -> FuncVar.find_defs_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var_defs query cilfile =
match query.str with Fun_s(funname) -> resolve_query_var_defs_fun query cilfile funname
                | None_s -> resolve_query_var_defs_none query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_var query cilfile = 
match query.f with Uses_f -> resolve_query_var_uses query cilfile
                | Decl_f -> resolve_query_var_decl query cilfile
                | Defs_f -> resolve_query_var_defs query cilfile 
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_return_none query cilfile =
match query.tar with Name_t(name) -> FuncFunction.find_returns name (-1) cilfile
                | ID_t(id) -> FuncFunction.find_returns "" id cilfile
                | All_t -> FuncFunction.find_returns_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_return query cilfile = 
match query.str with None_s -> resolve_query_fun_return_none query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_defs_none query cilfile =
match query.tar with Name_t(name) -> FuncFunction.find_def name (-1) cilfile
                | ID_t(id) -> FuncFunction.find_def "" id cilfile
                | All_t -> FuncFunction.find_def_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_defs query cilfile =
match query.str with None_s -> resolve_query_fun_defs_none query cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_uses_none query cilfile =
match query.tar with Name_t(name) -> FuncFunction.find_uses name (-1) cilfile
                | ID_t(id) -> FuncFunction.find_uses "" id cilfile 
                | All_t -> FuncFunction.find_uses_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_uses_fun query cilfile funname =
match query.tar with Name_t(name) -> FuncFunction.find_uses_in_fun name (-1) funname cilfile
                | ID_t(id) -> FuncFunction.find_uses_in_fun "" id funname cilfile
                | All_t -> FuncFunction.find_uses_in_fun_all funname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_uses_cond query cilfile = 
match query.tar with Name_t(name) -> FuncFunction.find_uses_cond name (-1) cilfile
                | ID_t(id) -> FuncFunction.find_uses_cond "" id cilfile
                | All_t -> FuncFunction.find_uses_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_noncond query cilfile =
match query.tar with Name_t(name) -> FuncFunction.find_uses_noncond name (-1) cilfile
                | ID_t(id) -> FuncFunction.find_uses_noncond "" id cilfile
                | All_t -> FuncFunction.find_uses_noncond_all cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_uses query cilfile = 
match query.str with None_s -> resolve_query_fun_uses_none query cilfile
                | Fun_s(funname) -> resolve_query_fun_uses_fun query cilfile funname 
                | Cond_s -> resolve_query_fun_uses_cond query cilfile
                | NonCond_s -> resolve_query_fun_noncond query cilfile

let resolve_query_fun_usesvar_fun query cilfile varname strucfunname =
match query.tar with Name_t(funname) -> FuncFunction.find_usesvar_in_fun funname (-1) strucfunname varname cilfile
                | ID_t(id) -> FuncFunction.find_usesvar_in_fun "" id strucfunname varname cilfile 
                | All_t -> FuncFunction.find_usesvar_in_fun_all strucfunname varname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_usesvar_none query cilfile varname =
match query.tar with Name_t(funname) -> FuncFunction.find_usesvar funname (-1) varname cilfile
                | ID_t(id) -> FuncFunction.find_usesvar "" id varname cilfile
                | All_t -> FuncFunction.find_usesvar_all varname cilfile
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun_usesvar query cilfile varname =
match query.str with Fun_s(strucfunname) -> resolve_query_fun_usesvar_fun query cilfile varname strucfunname
                | None_s -> resolve_query_fun_usesvar_none query cilfile varname
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

let resolve_query_fun query cilfile =
match query.f with Returns_f -> resolve_query_fun_return query cilfile
                | Defs_f -> resolve_query_fun_defs query cilfile
                | Uses_f -> resolve_query_fun_uses query cilfile
                | UsesWithVar_f(varname) -> resolve_query_fun_usesvar query cilfile varname
                | _ -> Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]

(* Main mapping function *)
let map_query query cilfile = if (query.lim != None_c) then (Printf.printf "Not supported yet.\n"; ("", loc_default, "", -1)::[]) else (
match query.k with Datatype_k -> resolve_query_datatype query cilfile
                | Var_k -> resolve_query_var query cilfile
                | Fun_k -> resolve_query_fun query cilfile )