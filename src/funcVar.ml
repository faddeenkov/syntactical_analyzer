open Cil
open Cabs2cil
open Hashtbl

(* Helper functions *)
let is_equal_varname_varid varinfo name id = if ((String.compare varinfo.vname name = 0) || (varinfo.vid = id)) then true else false

let rec delete_elem list s = 
match list with x::xs -> if (String.compare x s = 0) then (delete_elem xs s) else x::(delete_elem xs s)
            | [] -> []

let rec delete_duplicates list acc = 
match list with x::xs -> delete_duplicates (delete_elem xs x) (x::acc)
            | [] -> acc

let is_temporary id = Inthash.mem allTempVars id

(*
let find_all_cil_generated info varname loc =
let stringList = delete_duplicates (List.map (fun x -> match x with (EnvVar(envinfo),_) -> envinfo.vname | _ -> "") (Hashtbl.find_all absolutenv varname)) []
in let rec iter_list list = match list with x::xs -> if (String.compare x info.vname = 0) then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::(iter_list xs) else iter_list xs
                                        | [] -> []
in iter_list stringList *)

class var_search_in_expr varname varid loc result includeCallTmp : nopCilVisitor =
object(self)
inherit nopCilVisitor 
method vvrbl info = (if (is_equal_varname_varid info varname varid)&&(includeCallTmp || not (is_temporary info.vid)) then (result := (!result)@((info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::[])) else ());  SkipChildren
method vlval (h,o) = DoChildren
method vexpr exp = DoChildren
end

(* Finds a variable in an expression *)
let search_expression exp name loc varid includeCallTmp = 
let result = ref []
in let visitor = new var_search_in_expr name varid loc result includeCallTmp
in ignore (visitCilExpr visitor exp); !result

(* Finds a variable in a lhost *)
let search_lhost host name loc varid includeCallTmp = 
match host with Var(info) -> if (is_equal_varname_varid info name varid)&&(includeCallTmp || not (is_temporary info.vid)) then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::[] else []
            | Mem(exp) -> search_expression exp name loc varid includeCallTmp
 
let rec search_offset os name loc varid includeCallTmp =
match os with NoOffset -> []
            | Field(_, offset) -> search_offset offset name loc varid includeCallTmp
            | Index(exp, offset) -> (search_expression exp name loc varid includeCallTmp)@(search_offset offset name loc varid includeCallTmp)

(* Finds a variable in a list of expressions *)
let rec search_expression_list list name loc varid includeCallTmp = 
match list with x::xs -> (search_expression x name loc varid includeCallTmp)@(search_expression_list xs name loc varid includeCallTmp)
                | [] -> []

(* Finds a variable in a list of instructions *)
let rec search_instr_list_for_var list name varid includeCallTmp = 
match list with Set((lhost, offset), exp, loc)::xs -> (search_lhost lhost name loc varid includeCallTmp)@(search_offset offset name loc varid includeCallTmp)@(search_expression exp name loc varid includeCallTmp)@(search_instr_list_for_var xs name varid includeCallTmp)
                | VarDecl(info, loc)::xs ->  (match info.vtype with TArray(_, Some(exp), _) -> search_expression exp name loc varid includeCallTmp | _ -> [])@(if (is_equal_varname_varid info name varid) && (includeCallTmp || not (is_temporary info.vid)) then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::(search_instr_list_for_var xs name varid includeCallTmp) else (search_instr_list_for_var xs name varid includeCallTmp))
                | Call (Some(lhost,offset), exp, exp_list, loc)::xs -> (search_lhost lhost name loc varid includeCallTmp)@(search_offset offset name loc varid includeCallTmp)@(search_expression exp name loc varid includeCallTmp)@(search_expression_list exp_list name loc varid includeCallTmp)@(search_instr_list_for_var xs name varid includeCallTmp)
                | Call (None, exp, exp_list, loc)::xs -> (search_expression exp name loc varid includeCallTmp)@(search_expression_list exp_list name loc varid includeCallTmp)@(search_instr_list_for_var xs name varid includeCallTmp)
                (* Should I consider Asm too? *)
                | _::xs -> search_instr_list_for_var xs name varid includeCallTmp
                | [] -> []
 
 (* Finds a variable in a list of statements *)
let rec search_stmt_list_for_var list name varid includeCallTmp = 
match list with x::xs -> (match x.skind with Instr(ins_list) -> search_instr_list_for_var ins_list name varid includeCallTmp
                                            | Return(Some(exp), loc) -> search_expression exp name loc varid includeCallTmp
                                            | Goto(s_ref, loc) -> search_stmt_list_for_var ((!s_ref)::[]) name varid includeCallTmp
                                            | ComputedGoto(exp, loc) -> search_expression exp name loc varid includeCallTmp
                                            | If (exp, b1, b2, loc) -> (search_expression exp name loc varid includeCallTmp)@(search_stmt_list_for_var b1.bstmts name varid includeCallTmp)@(search_stmt_list_for_var b2.bstmts name varid includeCallTmp) 
                                            | Switch (exp, block, stmt_list, loc) -> (search_expression exp name loc varid includeCallTmp)@(search_stmt_list_for_var stmt_list name varid includeCallTmp) (* (search_stmt_list_for_var block.bstmts name)@ is this a duplicate of stmt list? *)
                                            | Loop (block, loc, None, None) -> search_stmt_list_for_var block.bstmts name varid includeCallTmp
                                            | Loop (block, loc, None, Some(s2)) -> (search_stmt_list_for_var block.bstmts name varid includeCallTmp)@(search_stmt_list_for_var (s2::[]) name varid includeCallTmp)
                                            | Loop (block, loc, Some(s1), None) -> (search_stmt_list_for_var block.bstmts name varid includeCallTmp)@(search_stmt_list_for_var (s1::[]) name varid includeCallTmp)
                                            | Loop (block, loc, Some(s1), Some(s2)) -> (search_stmt_list_for_var block.bstmts name varid includeCallTmp)@(search_stmt_list_for_var (s1::[]) name varid includeCallTmp)@(search_stmt_list_for_var (s2::[]) name varid includeCallTmp)
                                            | Block(block) -> Printf.printf "A Block\n"; search_stmt_list_for_var block.bstmts name varid includeCallTmp
                                            | TryFinally (b1, b2, loc) -> (search_stmt_list_for_var b1.bstmts name varid includeCallTmp)@(search_stmt_list_for_var b2.bstmts name varid includeCallTmp)
                                            | TryExcept(b1, (instr_list, exp), b2, loc) -> (search_stmt_list_for_var b1.bstmts name varid includeCallTmp)@(search_instr_list_for_var instr_list name varid includeCallTmp)@(search_expression exp name loc varid includeCallTmp)@(search_stmt_list_for_var b2.bstmts name varid includeCallTmp)
                                            | _ -> [])@(search_stmt_list_for_var xs name varid includeCallTmp)
            | [] -> []

(* Finds all uses of a variable in a function-body *)
let find_uses_in_fun_var dec name varid includeCallTmp = 
let rec iter_list list = 
match list with x::xs -> (search_stmt_list_for_var dec.sbody.bstmts x (-1) includeCallTmp)@(iter_list xs)
                | [] -> []
in (*Hashtbl.iter (fun a b -> Printf.printf "%s is mapped to %s\n" a b) varnameMapping ;*) if varid != (-1) then search_stmt_list_for_var dec.sbody.bstmts name varid includeCallTmp else iter_list (delete_duplicates (Hashtbl.find_all varnameMapping name) [])

(* Finds the function in which a variable shall be found *)
let rec find_uses_in_fun_find_fun list name varname varid includeCallTmp = 
match list with GFun(dec, loc)::xs -> if String.compare dec.svar.vname name = 0 then find_uses_in_fun_var dec varname varid includeCallTmp else find_uses_in_fun_find_fun xs name varname varid includeCallTmp
            | [] -> []
            | _::xs -> find_uses_in_fun_find_fun xs name varname varid includeCallTmp

(* Finds all uses of a variable in a function *)
let find_uses_in_fun varname varid funname file includeCallTmp = find_uses_in_fun_find_fun file.globals funname varname varid includeCallTmp

let rec find_all_glob_vars list = 
match list with GVar(info, _, _)::xs -> info.vid::(find_all_glob_vars xs)
            | _::xs -> find_all_glob_vars xs
            | [] -> []

(* Finds all uses of all global variables in a function *)
let find_uses_in_fun_all_glob funname file includeCallTmp = 
let id_list = find_all_glob_vars file.globals
in let rec iter_list l = 
match l with x::xs -> (find_uses_in_fun "" x funname file includeCallTmp)@(iter_list xs)
            | [] -> []
in iter_list id_list

let rec find_fundec globals funname = 
match globals with GFun(dec, _)::xs -> if (String.compare dec.svar.vname funname = 0) then Some(dec) else find_fundec xs funname
                | _::xs -> find_fundec xs funname
                | [] -> None

(* Finds all uses of all variables in a function *)
let find_uses_in_fun_all funname file includeCallTmp = 
let fundec_opt = find_fundec file.globals funname
in let rec iter_list l = 
match l with x::xs -> (find_uses_in_fun "" x funname file includeCallTmp)@(iter_list xs)
            | [] -> []
in
match fundec_opt with None -> []
                | Some(fundec) -> (find_uses_in_fun_all_glob funname file includeCallTmp)@(iter_list (List.map (fun x -> x.vid) fundec.sformals))@(iter_list (List.map (fun x -> x.vid) fundec.slocals))

let rec find_var_in_globals varname varid list =
match list with GVar(info, _, loc)::xs -> if is_equal_varname_varid info varname varid then [(info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)] else find_var_in_globals varname varid xs
            | _::xs -> find_var_in_globals varname varid xs
            | [] -> []

(* Find all uses of a variable in all functions *)
let find_uses varname varid file includeCallTmp = 
let rec find_uses_in_all_fun l = 
match l with GFun(dec, _)::xs -> (find_uses_in_fun varname varid dec.svar.vname file includeCallTmp)@(find_uses_in_all_fun xs)
            | _ ::xs -> find_uses_in_all_fun xs
            | [] -> []
in (find_var_in_globals varname varid file.globals)@(find_uses_in_all_fun file.globals)

(* Finds all uses of global variables in all functions *)
let find_uses_all_glob file includeCallTmp = 
let rec iter_functions list = 
match list with GFun(dec,_)::xs -> (find_uses_in_fun_all_glob dec.svar.vname file includeCallTmp)@(iter_functions xs)
            | _ ::xs -> iter_functions xs
            | [] -> []
in List.flatten ((List.map (fun x -> find_var_in_globals "" x file.globals) (find_all_glob_vars file.globals)))@(iter_functions file.globals)

(* Finds uses of all variables in all functions *)
let find_uses_all file includeCallTmp =
let rec iter_functions list = 
match list with GFun(dec,_)::xs -> (find_uses_in_fun_all dec.svar.vname file includeCallTmp)@(iter_functions xs)
            | _ ::xs -> iter_functions xs
            | [] -> []
in List.flatten ((List.map (fun x -> find_var_in_globals "" x file.globals) (find_all_glob_vars file.globals)))@(iter_functions file.globals)

let rec cond_search_uses_stmt_list list varname varid includeCallTmp =
match list with x::xs -> (match x.skind with If(exp, b1, b2, loc) -> (search_expression exp varname loc varid includeCallTmp)@(cond_search_uses_stmt_list (b1.bstmts@b2.bstmts) varname varid includeCallTmp)
                                        | Switch(exp, block, stmt_list, loc) -> (search_expression exp varname loc varid includeCallTmp)@(cond_search_uses_stmt_list block.bstmts varname varid includeCallTmp)
                                        | Loop(block, loc, None, None) -> (cond_search_uses_stmt_list block.bstmts varname varid includeCallTmp)
                                        | Loop (block, loc, None, Some(s1)) -> (cond_search_uses_stmt_list (s1::block.bstmts) varname varid includeCallTmp)
                                        | Loop(block, loc, Some(s2), None) -> (cond_search_uses_stmt_list (s2::block.bstmts) varname varid includeCallTmp)
                                        | Loop (block, loc, Some(s2), Some(s1)) -> (cond_search_uses_stmt_list (s2::s1::block.bstmts) varname varid includeCallTmp)
                                        | Block(block) -> (cond_search_uses_stmt_list block.bstmts varname varid includeCallTmp)
                                        | TryFinally (b1, b2, loc) -> (cond_search_uses_stmt_list (b1.bstmts@b2.bstmts) varname varid includeCallTmp)
                                        | TryExcept(b1, _, b2, loc) -> (cond_search_uses_stmt_list (b1.bstmts@b2.bstmts) varname varid includeCallTmp)
                                        | _ -> [] )@(cond_search_uses_stmt_list xs varname varid includeCallTmp)
            | [] -> []

(* Finds all uses of a variable in conditions of a function *)
let find_uses_in_cond_in_fun varname varid funname file includeCallTmp =
let fundec_opt = find_fundec file.globals funname
in let rec iter_list list fundec =
match list with x::xs -> (cond_search_uses_stmt_list fundec.sbody.bstmts x (-1) includeCallTmp)@(iter_list xs fundec)
            | [] -> []
in match fundec_opt with None -> []
| Some(fundec) -> if varid != (-1) then cond_search_uses_stmt_list fundec.sbody.bstmts varname varid includeCallTmp
else iter_list (delete_duplicates (Hashtbl.find_all varnameMapping varname) []) fundec

(* Finds all uses of a variable in conditions in all functions *)
let find_uses_in_cond varname varid file includeCallTmp = 
let rec iter_functions list = 
match list with GFun(dec,_)::xs -> (find_uses_in_cond_in_fun varname varid dec.svar.vname file includeCallTmp)@(iter_functions xs)
            | _ ::xs -> iter_functions xs
            | [] -> []
in iter_functions file.globals

(* Finds all uses of global variables in conditions in all functions *)
let find_uses_in_cond_all_glob file includeCallTmp = 
let id_list = find_all_glob_vars file.globals
in let rec iter_list list =
match list with x::xs -> (find_uses_in_cond "" x file includeCallTmp)@(iter_list xs)
            | [] -> []
in iter_list id_list

(* Finds all uses of global variables in conditions in a function *)
let find_uses_in_cond_in_fun_all_glob funname file includeCallTmp =
let id_list = find_all_glob_vars file.globals
in let rec iter_list list = 
match list with x::xs -> (find_uses_in_cond_in_fun "" x funname file includeCallTmp)@(iter_list xs)
            | [] -> []
in iter_list id_list

(* Finds all uses of variables in conditions in a function *)
let find_uses_in_cond_in_fun_all funname file includeCallTmp = 
let get_formals_locals dec = dec.sformals@dec.slocals
in let rec iter_list list = 
match list with x::xs -> (find_uses_in_cond_in_fun x.vname (-1) funname file includeCallTmp)@(iter_list xs)
| [] -> []
in let fundec_opt = find_fundec file.globals funname
in match fundec_opt with None -> []
| Some(fundec) -> (find_uses_in_cond_in_fun_all_glob funname file includeCallTmp)@(iter_list (get_formals_locals fundec))

(* Finds all uses of variables in conditions in all functions *)
let find_uses_in_cond_all file includeCallTmp =
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_uses_in_cond_in_fun_all dec.svar.vname file includeCallTmp)@(iter_list xs)
            | _ ::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

let rec remove_result list res =
match list with x::xs -> if x = res then (remove_result xs res) else x::(remove_result xs res)
            | [] -> []

(* Finds all uses of a variable in non-conditions *)
let find_uses_in_noncond varname varid file includeCallTmp =
let no_struc_result = find_uses varname varid file includeCallTmp
in let cond_result = find_uses_in_cond varname varid file includeCallTmp
in let rec iter_list list new_list = 
match list with x::xs -> iter_list xs (remove_result new_list x)
            | [] -> new_list
in iter_list cond_result no_struc_result

(* Finds all uses of global variables in non-conditions *)
let find_uses_in_noncond_all_glob file includeCallTmp =
let id_list = find_all_glob_vars file.globals
in let rec iter_list list =
match list with x::xs -> (find_uses_in_noncond "" x file includeCallTmp)@(iter_list xs)
            | [] -> []
in iter_list id_list

(* Finds all uses of variables in non-conditions *)
let find_uses_in_noncond_all file includeCallTmp =
let no_struc_result = find_uses_all file includeCallTmp
in let cond_result = find_uses_in_cond_all file includeCallTmp
in let rec iter_list list new_list = 
match list with x::xs -> iter_list xs (remove_result new_list x)
            | [] -> new_list
in iter_list cond_result no_struc_result

(* Finds the declaration of a variable in a function *)
let find_decl_in_fun varname varid funname file =
let fundec_opt = find_fundec file.globals funname
in let get_formals_locals dec = dec.sformals@dec.slocals
in let rec iter_list_id list = 
match list with x::xs -> if (x.vid = varid) then (x.vname, x.vdecl, (String.trim (Pretty.sprint 1 (d_type () x.vtype))), x.vid)::(iter_list_id xs) else iter_list_id xs
            | [] -> []
in let rec iter_list_name list name =
match list with x::xs -> if (String.compare x.vname name = 0) then (x.vname, x.vdecl, (String.trim (Pretty.sprint 1 (d_type () x.vtype))), x.vid)::(iter_list_name xs name) else iter_list_name xs name
            | [] -> []
in let rec iter_namelist name_list varinfo_list =
match name_list with x::xs -> (iter_list_name varinfo_list x)@(iter_namelist xs varinfo_list)
                | [] -> []
in match fundec_opt with None -> []
                    | Some(fundec) -> if varid != (-1) then iter_list_id (get_formals_locals fundec)
else iter_namelist (delete_duplicates (Hashtbl.find_all varnameMapping varname) []) (get_formals_locals fundec)

(* Finds all declarations in a function *)
let find_decl_in_fun_all funname file =
let fundec_opt = find_fundec file.globals funname
in let rec iter_list list =
match list with x::xs -> (x.vname, x.vdecl, (String.trim (Pretty.sprint 1 (d_type () x.vtype))), x.vid)::(iter_list xs)
            | [] -> []
in match fundec_opt with None -> []
                    | Some(fundec) -> iter_list (fundec.sformals@fundec.slocals)

(* Finds all global variable declarations *)
let find_decl_all_glob file = 
let rec iter_list list = 
match list with GVar(info, _, loc)::xs -> (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::(iter_list xs)          | _::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

(* Finds the declarations of a variable globally and in all functions  *)
let find_decl varname varid file = 
let rec iter_global_decls result =
match result with (name, loc, typ, id)::xs -> if (String.compare varname name = 0)|| id = varid then (name, loc, typ, id)::[] else iter_global_decls xs
            | [] -> []
in let rec iter_functions globals = 
match globals with GFun(dec, loc)::xs -> (find_decl_in_fun varname varid dec.svar.vname file)@(iter_functions xs)
                | _ ::xs -> iter_functions xs
                | [] -> []
in (iter_global_decls (find_decl_all_glob file))@(iter_functions file.globals)

(* Finds all declaration globally and in all functions *)
let find_decl_all file =
let rec iter_list list = 
match list with GFun(dec, loc)::xs -> (find_decl_in_fun_all dec.svar.vname file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in (find_decl_all_glob file)@(iter_list file.globals)

class var_find_def_in_fun varname varid funname result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vglob global = DoChildren
method vfunc fundec = if (String.compare fundec.svar.vname funname = 0) then DoChildren else SkipChildren
method vblock block = DoChildren
method vstmt stmt = DoChildren
method vinst instr = 
match instr with Set((Var(info),_), exp, loc) -> if (is_equal_varname_varid info varname varid) then (result := (!result)@((info.vname, loc, String.trim (Pretty.sprint 1 (d_type () info.vtype)), info.vid)::[]); SkipChildren) else SkipChildren
            | _ -> SkipChildren
end

(* Finds definitions of a variable in a function *)
let find_defs_in_fun varname varid funname file =
let result = ref [] 
in let rec iter_list list =
match list with x::xs -> visitCilFileSameGlobals (new var_find_def_in_fun x (-1) funname result) file; iter_list xs
            | [] -> !result
in let visitor = new var_find_def_in_fun varname varid funname result
in if varid != (-1) then (visitCilFileSameGlobals visitor file; !result)
else iter_list (delete_duplicates (Hashtbl.find_all varnameMapping varname) [])

(* Finds definitions of all global variables in a function *)
let find_defs_in_fun_all_glob funname file =
let rec iter_list list =
match list with GVar(info, _, _)::xs -> (find_defs_in_fun "" info.vid funname file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

(* Finds definitions of all variables in a functions *)
let find_defs_in_fun_all funname file =
let fundec_opt = find_fundec file.globals funname 
in let get_formals_locals dec = dec.sformals@dec.slocals
in let rec iter_list list =
match list with x::xs -> (find_defs_in_fun "" x.vid funname file)@(iter_list xs)
            | [] -> []
in match fundec_opt with None -> []
                    | Some(fundec) -> (find_defs_in_fun_all_glob funname file)@(iter_list (get_formals_locals fundec))

(* Finds definitions of a variable in all functions *)
let find_defs varname varid file = 
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_defs_in_fun varname varid dec.svar.vname file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in (find_var_in_globals varname varid file.globals)@(iter_list file.globals)

(* Finds definitions of all global variables in all functions *)
let find_defs_all_glob file =
let rec iter_list list =
match list with GFun(dec,_)::xs -> (find_defs_in_fun_all_glob dec.svar.vname file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in List.flatten ((List.map (fun x -> find_var_in_globals "" x file.globals) (find_all_glob_vars file.globals)))@(iter_list file.globals)

(* Finds definitions of all variables in all functions *)
let find_defs_all file =
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_defs_in_fun_all dec.svar.vname file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in List.flatten ((List.map (fun x -> find_var_in_globals "" x file.globals) (find_all_glob_vars file.globals)))@(iter_list file.globals)