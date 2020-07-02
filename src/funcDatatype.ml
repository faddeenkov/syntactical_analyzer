open Cil
open FuncVar

(* Finds the declaration of a user-defined type by name *)
let rec find_decl_iter_list name list = match list with (GCompTagDecl(info, loc))::xs -> if (String.compare name info.cname = 0) then ("", loc, name, -1)::[] else find_decl_iter_list name xs
                                                    | (GEnumTagDecl(info, loc))::xs -> if (String.compare name info.ename = 0) then ("", loc, name, -1)::[] else find_decl_iter_list name xs
                                                    | x::xs -> find_decl_iter_list name xs
                                                    | [] -> []

(* Finds the definition of a user-defined type by name *)
let rec find_userdef_iter_list name list = match list with (GType(info, loc))::xs -> if (String.compare name info.tname = 0) then ("", loc, name, -1)::[] else find_userdef_iter_list name xs
                                                    | (GCompTag(info, loc))::xs -> if (String.compare name info.cname = 0) then ("", loc, name, -1)::[] else find_userdef_iter_list name xs
                                                    | (GEnumTag(info, loc))::xs -> if (String.compare name info.ename = 0) then ("", loc, name, -1)::[] else find_userdef_iter_list name xs
                                                    | x::xs -> find_userdef_iter_list name xs
                                                    | [] -> [] 

(* Finds declaration of user-defined datatypes *)
let find_decl name file = find_decl_iter_list name file.globals

(* Finds definition of a user-defined type *)
let find_def name file = find_userdef_iter_list name file.globals

(* Finds all definition of user-defined types *)
let find_def_all file = 
let rec iter_list list = 
match list with GType(info, loc)::xs -> ("", loc, info.tname, -1)::(iter_list xs)
            | GCompTag(info, loc)::xs -> ("", loc, info.cname, -1)::(iter_list xs)
            | GEnumTag(info, loc)::xs -> ("", loc, info.ename, -1)::(iter_list xs)
            | x::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

(* Finds all global user-defined types and their type-definition *)
let rec find_decl_all_glob_iter_list list acc = 
match list with (GCompTagDecl(info,loc))::xs -> find_decl_all_glob_iter_list xs (("", loc, info.cname, -1)::acc)
            | GEnumTagDecl(info, loc)::xs -> find_decl_all_glob_iter_list xs (("", loc, info.ename, -1)::acc)
            | x::xs -> find_decl_all_glob_iter_list xs acc
            | [] -> acc

(* Finds all global type-definitions of user-defined variables *)
let find_decl_all_glob file = List.rev (find_decl_all_glob_iter_list file.globals [])

(* In CIL you cannot define any new datatype in function-scopes (see CIL-Doc 4.5) *)
let find_decl_all file = find_decl_all_glob file

let rec find_in_globals list name = 
match list with GVar(info,t,loc)::xs -> if (String.compare name (String.trim (Pretty.sprint 1 (d_type () info.vtype))) = 0) then info.vid::(find_in_globals xs name) else (find_in_globals xs name)
                | [] -> []
                | _::xs -> find_in_globals xs name

let rec find_in_varinfos list name = 
match list with info::xs -> if (String.compare name (String.trim (Pretty.sprint 1 (d_type () info.vtype))) = 0) then info.vid::(find_in_varinfos xs name) else (find_in_varinfos xs name)
                | [] -> []

let rec find_fundec globals name = 
match globals with GFun(fundec, _)::xs -> if (String.compare fundec.svar.vname name = 0) then Some(fundec) else find_fundec xs name
                | [] -> None
                | _::xs -> find_fundec xs name

let rec find_typevar_uses_in_fun list funname file = 
match list with x::xs -> (FuncVar.find_uses_in_fun "" x funname file false)@(find_typevar_uses_in_fun xs funname file)
            | [] -> []

(* Finds uses of a datatype in a function *)
let find_uses_in_fun typename funname file = 
let fundec_res = find_fundec file.globals funname
in match fundec_res with None -> []
                    | Some(f) -> find_typevar_uses_in_fun ((find_in_globals file.globals typename)@(find_in_varinfos f.slocals typename)@(find_in_varinfos f.sformals typename)) f.svar.vname file

(* Finds all uses of a datatype in all functions *)
let find_uses typename file = 
let list = FuncVar.find_uses_all file false
in List.filter (fun (name, loc, typ, id) -> (String.compare typ typename = 0)) list

(* Finds all uses of a datatype in conditions *)
let find_uses_in_cond typename file =
let list = FuncVar.find_uses_in_cond_all file false
in List.filter (fun (name, loc, typ, id) -> (String.compare typ typename = 0)) list

(* Finds all uses of a datatype in non-conditions *)
let find_uses_in_noncond typename file =
let list = FuncVar.find_uses_in_noncond_all file false
in List.filter (fun (name, loc, typ, id) -> (String.compare typ typename = 0)) list