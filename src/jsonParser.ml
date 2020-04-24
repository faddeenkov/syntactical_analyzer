open Yojson.Safe
(*
open Cil
open Frontc *)

(* JSON-query has the following form:
        select: ...
        type: ...
        target: ...
        find: ...
        structure: ...
        constraint: ... *)

(* Definition of parameters *)
(* type select_der = string list

type name_t = {name : string}
type id_t = {id : int}
type target_der = name_t
| id_t *)

type selectable = Name_sel [@name "name"]
| Location_sel [@name "location"]
| Type_sel [@name "type"]
| ID_sel [@name "id"]
[@@deriving yojson]

type select = selectable list
[@@deriving yojson]

type kind = Var_k [@name "var"]
| Fun_k [@name "fun"]
| Datatype_k [@name "datatype"]
[@@deriving yojson]

type target = Name_t of string [@name "name"]
| ID_t of int [@name "id"]
| All_t [@name "$all"]
| AllGlobVar_t [@name "$all_glob_var"]
| Or_t of (string list) [@name "or"]
| And_t of (string list) [@name "and"]
[@@deriving yojson]

type find = Uses_f [@name "uses"]
| Decl_f [@name "decl"]
| Defs_f [@name "defs"]
| UsesWithVar_f of string [@name "uses_with_var"]
| Returns_f [@name "returns"]
[@@deriving yojson]

type structure = Fun_s of string [@name "fun_name"]
| Cond_s [@name "cond"]
| NonCond_s [@name "non-cond"]
| None_s [@name "$none"]
[@@deriving yojson]

type constr = Constraint_c of string [@name "constr"]
| None_c [@name "$none"]
[@@deriving yojson]


exception SyntaxError of string

(* Generate select-parameter *)

let rec resolve_sel_helper acc l = match l with [] -> acc
                                    | `String(x)::xs -> (match (String.lowercase_ascii x) with "name" -> (resolve_sel_helper (Name_sel::acc) xs)
                                                                                            | "location" -> (resolve_sel_helper (Location_sel::acc) xs)
                                                                                            | "type" -> (resolve_sel_helper (Type_sel::acc) xs)
                                                                                            | "id" -> (resolve_sel_helper (ID_sel::acc) xs)
                                                                                            | _ -> raise (SyntaxError "Wrong syntax: unknown selectable"))
                                    | __-> raise (SyntaxError "Impossible error")

let resolve_sel_json j = match j with `String(s) -> if (String.compare (String.lowercase_ascii s) "$all" = 0) then [Name_sel; Location_sel; Type_sel; ID_sel] else raise (SyntaxError "Wrong syntax: unexpected input for selection")
                                    | `List(x::xs) -> List.rev (resolve_sel_helper [] (x::xs))
                                    | _ -> raise (SyntaxError "Wrong syntax: unexpected input for selection")

(* Generate type-parameter *)

let resolve_type_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "var" -> Var_k
                                                                                            | "fun" -> Fun_k
                                                                                            | "datatype" -> Datatype_k
                                                                                            | _ -> raise (SyntaxError "Wrong syntax: unexpected input for type"))
                                         | _ -> raise (SyntaxError "Wrong syntax: unexpected input for type")

(* Generate target-parameter *)

let resolve_target_name j = match j with `String(s) -> s
                                        | _ -> raise (SyntaxError "Wrong syntax: name-input should be a string")

let resolve_target_id j = match j with `Int(s) -> s
                                        | _ -> raise (SyntaxError "Wrong syntax: ID-input should be an integer")

let resolve_target_or_and j = match j with `List(l) -> (List.map (fun a -> match a with `String(s) -> s | _ -> raise (SyntaxError "Wrong syntax: Element in or-list is not a string") ) l)
                                        | _ -> []

let resolve_target_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "$all" -> All_t
                                                                                        | "$all_glob_var" -> AllGlobVar_t
                                                                                        | _ -> raise (SyntaxError "Wrong syntax: Unexpected string as target-input"))
                                        | `Assoc(l) -> (match l with ((s,j)::[]) -> (match (String.lowercase_ascii s) with "id" -> ID_t(resolve_target_id j)
                                                                                                                        | "or" -> Or_t(resolve_target_or_and j)
                                                                                                                        | "and" -> And_t(resolve_target_or_and j)
                                                                                                                        | "name" -> Name_t(resolve_target_name j)
                                                                                                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected form of association in input for target"))
                                                                | _ -> raise (SyntaxError "Wrong syntax: unexpected form of association in input for target"))
                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for target")

(* Generate find-parameter *)

let resolve_find_uses_with_var j = match j with `String(s) -> s
                                                | _ -> raise (SyntaxError "Wrong syntax: unexpected argument for uses_with_var in find")

let resolve_find_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "uses" -> Uses_f
                                                                                        | "decl" -> Decl_f
                                                                                        | "defs" -> Defs_f
                                                                                        | "returns" -> Returns_f
                                                                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for find"))
                                        | `Assoc(l) -> (match l with ((s, json)::[]) -> if (String.compare (String.lowercase_ascii s) "uses_with_var" = 0) then UsesWithVar_f(resolve_find_uses_with_var json) else raise (SyntaxError "Wrong syntax: unexpected input for find")
                                                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for find in assoc"))
                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for find")

(* Generate structure-parameter *)

let resolve_struc_fun_name j = match j with `String(s) -> s
                                        | _ -> raise (SyntaxError "Wrong syntax: fun_name parameter in strucutre has to a string")

let resolve_struc_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "$none" -> None_s
                                                                                        | "cond" -> Cond_s
                                                                                        | "non-cond" -> NonCond_s
                                                                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for structure"))
                                        | `Assoc(l) -> (match l with ((s, json)::[]) -> if (String.compare (String.lowercase_ascii s) "fun_name" = 0) then Fun_s(resolve_struc_fun_name json) else raise (SyntaxError "Wrong syntax: unexpected input for structure")
                                                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for structure"))
                                        | _ -> raise (SyntaxError "Wrong syntax: unexpected input for structure")

(* Generate constraint-parameter *)

let resolve_constr_json j = match j with `String(s) -> (match (String.lowercase_ascii s) with "$none" -> None_c
                                                                                                | _ -> raise (SyntaxError "Wrong syntax: unexpected string as constraint-input"))
                                        | `Assoc(l) -> (match l with ((s, `String(c))::[]) -> if(String.compare (String.lowercase_ascii s) "constr" = 0) then Constraint_c(c) else raise (SyntaxError "Wrong syntax: unexpected association in constraint")
                                                                        | _ -> raise (SyntaxError "Syntax error in constraint"))
                                        | _ -> raise (SyntaxError "Wrong syntax: parameter of constraint must be a string")

(* Type-definition of a query for mapping use *)
type query = {sel : select; [@key "select"] [@of_yojson fun x -> Result.Ok(resolve_sel_json x)]
              k : kind; [@key "type"] [@of_yojson fun x -> Result.Ok(resolve_type_json x)]
              tar : target; [@key "target"] [@of_yojson fun x -> Result.Ok(resolve_target_json x)]
              f : find; [@key "find"] [@of_yojson fun x -> Result.Ok(resolve_find_json x)]
              str : (structure [@default None_s]); [@key "structure"] [@of_yojson fun x -> Result.Ok(resolve_struc_json x)]
              lim : (constr [@default None_c]); [@key "constraint"] [@of_yojson fun x -> Result.Ok(resolve_constr_json x)]
              }
              [@@deriving yojson]

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

exception Error of string

let parse_json_file filename =
let jsonTree = from_file filename
in let derived = query_of_yojson jsonTree
in match derived with Result.Ok y -> y
                    | Result.Error x -> raise (Error(x))
