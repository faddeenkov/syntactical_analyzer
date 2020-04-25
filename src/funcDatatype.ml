open Cil

(* Finds the declaration of a user-defined type by name *)
let rec find_decl_iter_list name list = match list with (GCompTagDecl(info, loc))::xs -> if (String.compare name info.cname = 0) then Some (loc, name) else find_decl_iter_list name xs
                                                    | (GEnumTagDecl(info, loc))::xs -> if (String.compare name info.ename = 0) then Some (loc, name) else find_decl_iter_list name xs
                                                    | x::xs -> find_decl_iter_list name xs
                                                    | [] -> None

(* Finds the definition of a user-defined type by name *)
let rec find_userdef_iter_list name list = match list with (GType(info, loc))::xs -> if (String.compare name info.tname = 0) then Some (loc, name) else find_userdef_iter_list name xs
                                                    | (GCompTag(info, loc))::xs -> if (String.compare name info.cname = 0) then Some (loc, name) else find_userdef_iter_list name xs
                                                    | (GEnumTag(info, loc))::xs -> if (String.compare name info.ename = 0) then Some (loc, name) else find_userdef_iter_list name xs
                                                    | x::xs -> find_userdef_iter_list name xs
                                                    | [] -> None

(* Builds a list out of the results of type-definition/declaration search *)
let find_decl_helper name file = let result_userdef = find_userdef_iter_list name file.globals
in
let loc_default = {line = -1; file = ""; byte = -1}
in
let result_decl = find_decl_iter_list name file.globals
in if result_userdef = None then (if result_decl = None then (loc_default,"") else (match result_decl with (Some(res)) -> res
                                                                                                            | _ -> (loc_default, ""))) else (match result_userdef with Some(res) -> res
                                                                                                                                                                  | _ -> (loc_default, "")  )
(* Finds type-definition or declaration of user-defined datatypes *)
let find_decl name file = match (find_decl_helper name file) with (loc, typename) -> ("", loc, typename, -1)

(* Finds all global user-defined types and their type-definition *)
let rec find_decl_all_glob_iter_list list acc = 
match list with (GType(info, loc))::xs -> find_decl_all_glob_iter_list xs (("", loc, info.tname, -1)::acc)
            | (GCompTag(info,loc))::xs -> find_decl_all_glob_iter_list xs (("", loc, info.cname, -1)::acc)
            | GEnumTag(info, loc)::xs -> find_decl_all_glob_iter_list xs (("", loc, info.ename, -1)::acc)
            | x::xs -> find_decl_all_glob_iter_list xs acc
            | [] -> acc

(* Finds all global type-definitions of user-defined variables *)
let find_decl_all_glob file = List.rev (find_decl_all_glob_iter_list file.globals [])

(* In CIL you cannot define any new datatype in function-scopes (see CIL-Doc 4.5) *)
let find_decl_all file = find_decl_all_glob file

(* let rec find_in_globals list name = 
match list with GVarDecl(info, _) -> if (String.compare name (String.trim (Pretty.sprint 1 (d_type () info.vtype)))) -> info.vid *)

(* Find uses of a datatype in a function *)
let find_uses_in_fun typename funname file = [] (* This function needs to use variable-search in functions. *) 