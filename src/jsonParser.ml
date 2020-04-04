open Yojson.Basic
open Cil
open Frontc

module JSONParser =
struct

(* JSON-query has the following form:
        select: ...
        type: ...
        target: ...
        find: ...
        struture: ...
        constraint: ... *)

(* Definition of parameters *)
type selectable = Name_sel
| Location_sel
| Type_sel
| ID_sel

type select = selectable list

type kind = Var_k
| Fun_k
| Datatype_k

type target = Name_t of string
| ID_t of int
| All_t
| AllGlobVar_t
| Or_t of (string list)
| And_t of (string list)

type find = Uses_f
| Decl_f
| Defs_f
| UsesWithVar_f of string
| Returns_f

type structure = Fun_s of string
| Cond_s
| NonCond_s
| None_s

type constr = Constraint_c of string
| None_c

type query = {sel : select;
              k : kind;
              tar : target;
              f : find;
              str : structure;
              lim : constr;
              }
              
(* Generates query from JSON-tree *)

let rec get_parameter_json l param = match l with (s,j)::xs -> if (String.compare (String.lowercase_ascii s) (String.lowercase_ascii param) = 0) then j else get_parameter_json xs param
                                                  | _ -> Printf.printf "Error: parameter was not found\n"; `Null

(* Generate select-parameter *) 

let rec resolve_sel_helper acc l = match l with [] -> acc
                                    | `String(x)::xs -> (match (String.lowercase_ascii x) with "name" -> (resolve_sel_helper (Name_sel::acc) xs)
                                                                                            | "location" -> (resolve_sel_helper (Location_sel::acc) xs)
                                                                                            | "type" -> (resolve_sel_helper (Type_sel::acc) xs)
                                                                                            | "id" -> (resolve_sel_helper (ID_sel::acc) xs)
                                                                                            | _ -> (Printf.printf "Wrong syntax: unknown selectable\n"; (resolve_sel_helper acc xs)))
                                    | __-> Printf.printf "Impossible error\n"; []

let resolve_sel_json j = match j with `String(s) -> if (String.compare (String.lowercase_ascii s) "$all" = 0) then [Name_sel; Location_sel; Type_sel; ID_sel] else (Printf.printf "Wrong syntax: unexpected input for selection\n"; [])
                                    | `List(x::xs) -> resolve_sel_helper [] (x::xs)
                                    | _ -> Printf.printf "Wrong syntax: unexpected input for selection\n"; []

let generate_select tree = match tree with `Assoc(attr_list) -> resolve_sel_json (get_parameter_json attr_list "select")
                                            | _ ->  Printf.printf "Wrong syntax: association expected\n";[]

(* Generate type-parameter *)

let resolve_type_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "var" -> Var_k
                                                                                            | "fun" -> Fun_k
                                                                                            | "datatype" -> Datatype_k
                                                                                            | _ -> Printf.printf "Wrong syntax: unexpected input for type\n"; Var_k)
                                         | _ -> Printf.printf "Wrong syntax: unexpected input for type\n"; Var_k

let generate_type tree = match tree with `Assoc(attr_list) -> resolve_type_json (get_parameter_json attr_list "type")
                                        | _ -> Printf.printf "Wrong syntax: association expected\n"; Var_k

(* Generate target-parameter *)

let resolve_target_id j = match j with `String(s) -> int_of_string s
                                        | _ -> -1

let resolve_target_or_and j = match j with `List(l) -> (List.map (fun a -> match a with `String(s) -> s | _ -> Printf.printf "Wrong syntax: Element in or-list is not a string\n"; "" ) l)
                                        | _ -> []

let resolve_target_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "$all" -> All_t
                                                                                        | "$all_glob_var" -> AllGlobVar_t
                                                                                        | _ -> Name_t(s))
                                        | `Assoc(l) -> (match l with ((s,j)::[]) -> (match (String.lowercase_ascii s) with "id" -> ID_t(resolve_target_id j)
                                                                                                                        | "or" -> Or_t(resolve_target_or_and j)
                                                                                                                        | "and" -> And_t(resolve_target_or_and j)
                                                                                                                        | _ -> Printf.printf "Wrong syntax: unexpected form of association in input for target\n"; All_t)
                                                                | _ -> Printf.printf "Wrong syntax: unexpected form of association in input for target\n"; All_t)
                                        | _ -> Printf.printf "Wrong syntax: unexpected input for target\n"; All_t

let generate_target tree = match tree with `Assoc(attr_list) -> resolve_target_json (get_parameter_json attr_list "target")
                                            | _ -> Printf.printf "Wrong syntax: association expected\n"; All_t

(* Generate find-parameter *)

let resolve_find_uses_with_var j = match j with `String(s) -> s
                                                | _ -> Printf.printf "Wrong syntax: unexpected argument for uses_with_var in find\n"; ""

let resolve_find_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "uses" -> Uses_f
                                                                                        | "decl" -> Decl_f
                                                                                        | "defs" -> Defs_f
                                                                                        | "returns" -> Returns_f
                                                                                        | _ -> Printf.printf "Wrong syntax: unexpected input for find\n"; Uses_f)
                                        | `Assoc(l) -> (match l with ((s, json)::[]) -> if (String.compare (String.lowercase_ascii s) "uses_with_var" = 0) then UsesWithVar_f(resolve_find_uses_with_var json) else (Printf.printf "Wrong syntax: unexpected input for find\n"; Uses_f)
                                                                        | _ -> Printf.printf "Wrong syntax: unexpected input for find in assoc\n"; Uses_f)
                                        | _ -> Printf.printf "Wrong syntax: unexpected input for find\n"; Uses_f

let generate_find tree = match tree with `Assoc(attr_list) -> resolve_find_json (get_parameter_json attr_list "find")
                                        | _ -> Printf.printf "Wrong syntax: association expected\n"; Uses_f

(* Generate structure-parameter *)

let resolve_struc_fun_name j = match j with `String(s) -> s
                                        | _ -> Printf.printf "Wrong syntax: fun_name parameter in strucutre has to a string\n"; ""

let resolve_struc_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "$none" -> None_s
                                                                                        | "cond" -> Cond_s
                                                                                        | "non-cond" -> NonCond_s
                                                                                        | _ -> Printf.printf "Wrong syntax: unexpected input for structure\n"; None_s)
                                        | `Assoc(l) -> (match l with ((s, json)::[]) -> if (String.compare (String.lowercase_ascii s) "fun_name" = 0) then Fun_s(resolve_struc_fun_name json) else (Printf.printf "Wrong syntax: unexpected input for structure\n"; None_s)
                                                                        | _ -> Printf.printf "Wrong syntax: unexpected input for structure\n"; None_s)
                                        | _ -> Printf.printf "Wrong syntax: unexpected input for structure\n"; None_s

let generate_structure tree = match tree with `Assoc(attr_list) -> if (get_parameter_json attr_list "structure" = `Null) then None_s else resolve_struc_json (get_parameter_json attr_list "structure")
                                                | _ -> Printf.printf "Wrong syntax: association expected\n"; None_s

(* Generate constraint-parameter *)

let resolve_constr_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "$none" -> None_c
                                                                                                | _ -> Constraint_c(s))
                                        | _ -> Printf.printf "Wrong syntax: parameter of constraint should be a string\n"; None_c

let generate_constraint tree = match tree with `Assoc(attr_list) -> if (get_parameter_json attr_list "constraint" = `Null) then None_c else resolve_constr_json (get_parameter_json attr_list "constraint")
                                                | _ -> Printf.printf "Wrong syntax: association expected\n"; None_c

(* Generate parameter-structure *)

let generate_query tree = {sel = (generate_select tree);
                           k = (generate_type tree);
                           tar = (generate_target tree);
                           f = (generate_find tree);
                           str = (generate_structure tree);
                           lim = generate_constraint tree;} 

(* toString-function for query *)

let to_string_selectable x = match x with Name_sel -> "NAME"
                                        | Location_sel -> "LOCATION"
                                        | ID_sel -> "ID"
                                        | Type_sel -> "TYPE"

let rec to_string_sel sel = match sel with x::[] -> to_string_selectable x
                                        | x::xs -> (to_string_selectable x)^", "^(to_string_sel xs)
                                        | [] -> ""

let to_string_type k = match k with Var_k -> "var"
                                | Fun_k -> "fun"
                                | Datatype_k -> "datatype"

let rec to_string_target_or_and_list l = match l with x::[] -> x^" "
                                                | x::xs -> x^", "^(to_string_target_or_and_list xs)
                                                | [] -> ""

let to_string_target t = match t with All_t -> "$all"
                                | AllGlobVar_t -> "$all_glob"
                                | Name_t(s) -> "name: "^s
                                | ID_t(i) -> "ID: "^(string_of_int i)
                                | Or_t(l) -> "or: "^(to_string_target_or_and_list l)
                                | And_t(l) -> "and: "^(to_string_target_or_and_list l)

let to_string_find t = match t with Uses_f -> "uses"
                                | Decl_f -> "decl"
                                | Defs_f -> "defs"
                                | UsesWithVar_f(s) -> "uses_with_var: "^s
                                | Returns_f -> "returns"

let to_string_struc t = match t with Fun_s(s) -> "fun_name: "^s
                                | Cond_s -> "cond"
                                | NonCond_s -> "non-cond"
                                | None_s -> "none"

let to_string_constr t = match t with Constraint_c(s) -> "constraint: "^s
                                | None_c -> "none"

let to_string tree = "{sel = ["^(to_string_sel tree.sel)^"];\nk = "^(to_string_type tree.k)^";\ntar = "^(to_string_target tree.tar)^";\nf = "^(to_string_find tree.f)^";\nstr = "^(to_string_struc tree.str)^";\nlim = "^(to_string_constr tree.lim)^";}\n"

let parse_json_file filename = 
let jsonTree = from_file filename
in generate_query jsonTree

end


open JSONParser

(* Reads input *)
let filename = Sys.argv.(1)

let _ =
let query = parse_json_file filename
in Printf.printf "%s" (to_string query)