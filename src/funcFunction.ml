open Cil
open Cabs2cil
open Hashtbl

let is_equal_funname_funid varinfo name id = if ((String.compare varinfo.vname name = 0) || (varinfo.vid = id)) then true else false

let rec delete_elem list s = 
match list with x::xs -> if (String.compare x s = 0) then (delete_elem xs s) else x::(delete_elem xs s)
            | [] -> []

let rec delete_duplicates list acc = 
match list with x::xs -> delete_duplicates (delete_elem xs x) (x::acc)
            | [] -> acc


class fun_find_returns funname funid result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vfunc fundec = if is_equal_funname_funid fundec.svar funname funid then DoChildren else SkipChildren
method vstmt stmt = 
match stmt.skind with Return(Some(exp), loc) -> result := (!result)@(("", loc, String.trim (Pretty.sprint 1 (d_type () (typeOf exp))), (-1))::[]); DoChildren
                    | Return(None, loc) -> result := (!result)@(("", loc, "void", (-1))::[]); DoChildren
                    | _ -> DoChildren
end

(* Finds all returns of a function *)
let find_returns funname funid file =
let result = ref []
in let visitor = new fun_find_returns funname funid result
in ignore (visitCilFileSameGlobals visitor file); !result

(* Finds all returns in all functions *)
let find_returns_all file = 
let rec iter_list list = 
match list with GFun(fundec, _)::xs -> (find_returns "" fundec.svar.vid file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

let create_sig fundec file = 
let return_type = 
match (find_returns "" fundec.svar.vid file) with (_, _, typ, _)::xs -> typ
                                                | [] -> Printf.printf "This should never happen\n"; ""
in let rec input_type list = 
match list with x::[] -> (String.trim (Pretty.sprint 1 (d_type () x.vtype)))^" "^x.vname
            | x::xs -> (String.trim (Pretty.sprint 1 (d_type () x.vtype)))^" "^x.vname^", "^(input_type xs)
            | [] -> ""
in return_type^" "^fundec.svar.vname^" ("^(input_type fundec.sformals)^")"

(* Finds all definitions of a function *)
let find_def funname funid file =
let rec iter_list list =
match list with GFun(dec, loc)::xs -> if is_equal_funname_funid dec.svar funname funid then (dec.svar.vname, loc, create_sig dec file, dec.svar.vid)::(iter_list xs) else iter_list xs
            | _::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

(* Finds all definitions of all functions *)
let find_def_all file = 
let rec iter_list list =
match list with GFun(fundec, _)::xs -> (find_def "" fundec.svar.vid file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

let rec find_fundec funname funid list =
match list with GFun(dec, _)::xs -> if is_equal_funname_funid dec.svar funname funid then Some (dec) else find_fundec funname funid xs
            | _::xs -> find_fundec funname funid xs
            | [] -> None

class fun_find_uses funname funid file result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vinst instr = 
match instr with Call(_, Lval(Var(varinfo) ,NoOffset), list,loc) -> 
if is_equal_funname_funid varinfo funname funid then ( match (find_fundec funname funid file.globals) with None -> SkipChildren
                                                                        | Some (dec) -> result := (!result)@((varinfo.vname, loc, create_sig dec file, varinfo.vid)::[]); SkipChildren) else SkipChildren
              
            | _ -> SkipChildren
end

(* Finds all calls of a function in all functions *)
let find_uses funname funid file = 
let result = ref []
in let visitor = new fun_find_uses funname funid file result
in ignore (visitCilFileSameGlobals visitor file); !result 

(* Find all calls of all functions in all functions *)
let find_uses_all file = 
let rec iter_list list = 
match list with GFun(fundec, _)::xs -> (find_uses "" fundec.svar.vid file)@(iter_list xs)
            | _ ::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

class fun_find_uses_in_fun funname funid funstrucname file result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vfunc fundec = if is_equal_funname_funid fundec.svar funstrucname (-1) then DoChildren else SkipChildren
method vinst instr = 
match instr with Call(_, Lval(Var(varinfo) ,NoOffset), list,loc) -> 
if is_equal_funname_funid varinfo funname funid then ( match (find_fundec funname funid file.globals) with None -> SkipChildren
                                                                        | Some (dec) -> result := (!result)@((varinfo.vname, loc, create_sig dec file, varinfo.vid)::[]); SkipChildren) else SkipChildren
              
            | _ -> SkipChildren
end

(* Finds calls of a function in a function *)
let find_uses_in_fun funname funid funstrucname file =
let result = ref []
in let visitor = new fun_find_uses_in_fun funname funid funstrucname file result
in ignore (visitCilFileSameGlobals visitor file); !result

(* Finds all calls of all functions in a function *)
let find_uses_in_fun_all funstrucname file =
let rec iter_list list =
match list with GFun(fundec, _)::xs -> (find_uses_in_fun "" fundec.svar.vid funstrucname file)@(iter_list xs)
            | _::xs -> iter_list xs
            | [] -> []
in iter_list file.globals

let loc_default = {line = -1; file = ""; byte = -1}

class fun_find_usesvar_in_fun fundec funstrucname varname varid file result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vfunc dec = if is_equal_funname_funid dec.svar funstrucname (-1) then DoChildren else SkipChildren
method vinst instr =
match instr with Call(_, exp, list, loc) -> (match exp with Lval(Var(varinfo), _) -> if is_equal_funname_funid varinfo fundec.svar.vname fundec.svar.vid then (if List.length (FuncVar.search_expression_list list varname loc_default varid true) > 0 then (result := (!result)@((varinfo.vname, loc, create_sig fundec file, varinfo.vid)::[]); SkipChildren) else SkipChildren) else SkipChildren
                                                        | _ -> Printf.printf "some other exp in call\n"; SkipChildren)
                 | _ -> SkipChildren
end 

(* Finds calls of a function with a var in argument in a function *)
let find_usesvar_in_fun funname funid funstrucname varname file =
let fundec_opt = find_fundec funname funid file.globals
in let result = ref []
in let rec iter_list list fundec =
match list with x::xs -> ignore(visitCilFileSameGlobals (new fun_find_usesvar_in_fun fundec funstrucname x (-1) file result) file); iter_list xs fundec
        | [] -> !result
in match fundec_opt with None -> []
| Some(fundec) ->  iter_list (delete_duplicates (Hashtbl.find_all varnameMapping varname) []) fundec

(* Finds calls of all function with a var in argument in a function *)
let find_usesvar_in_fun_all funstrucname varname file =
let rec iter_list list = 
match list with GFun(dec, _)::xs -> (find_usesvar_in_fun "" dec.svar.vid funstrucname varname file)@(iter_list xs)
        | _::xs -> iter_list xs
        | [] -> []
in iter_list file.globals

(* Finds all calls of a function with a var in argument in all functions *)
let find_usesvar funname funid varname file =
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_usesvar_in_fun funname funid dec.svar.vname varname file)@(iter_list xs)
        | _::xs -> iter_list xs
        | [] -> []
in iter_list file.globals

(* Finds all calls of all functions with a var in argument in all functions *)
let find_usesvar_all varname file =
let rec iter_list list = 
match list with GFun(dec,_)::xs -> (find_usesvar "" dec.svar.vid varname file)@(iter_list xs)
        | _::xs -> iter_list xs
        | [] -> []
in iter_list file.globals

class find_calls_with_tmp file result funname funid : nopCilVisitor =
object
inherit nopCilVisitor
method vinst instr = 
match instr with Call(lval_opt, Lval(Var(varinfo) ,_), _, loc) -> if is_equal_funname_funid varinfo funname funid then (match lval_opt with Some((Var(tmpinfo), _)) -> if (String.length tmpinfo.vname > 2)&&(String.compare "tmp" (String.sub tmpinfo.vname 0 3) = 0) then result := (!result)@((tmpinfo.vid, varinfo.vid)::[]); SkipChildren
                                                                                | _ -> SkipChildren) else SkipChildren
        | _ -> SkipChildren
end

let find_lval_of_calls funname funid file =
let result = ref []
in let visitor = new find_calls_with_tmp file result funname funid
in visitCilFileSameGlobals visitor file; !result

let create_fun_res name id file loc =
let fundec_opt = find_fundec name id file.globals
in match fundec_opt with None -> ("", loc_default, "", -1)
                | Some(fundec) -> (fundec.svar.vname, loc, create_sig fundec file , fundec.svar.vid)

(* Finds all calls of a function in a condition in all functions *)
let find_uses_cond funname funid file =
let id_list = find_lval_of_calls funname funid file
in let rec iter_list list = 
match list with (tmp, func)::xs -> (match FuncVar.find_uses_in_cond "" tmp file true with (name, loc, typ, id)::ys -> (create_fun_res "" func file loc)::(iter_list xs)
                                                                        | [] -> iter_list xs)
        | _ -> []
in iter_list id_list

(* Finds all calls of all functions in a condition in all functions *)
let find_uses_cond_all file = 
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_uses_cond "" dec.svar.vid file)@(iter_list xs)
        | _::xs -> iter_list xs
        | [] -> []
in iter_list file.globals

let rec remove_result list res =
match list with x::xs -> if x = res then (remove_result xs res) else x::(remove_result xs res)
            | [] -> []

(* Finds calls of a function in non-condition in all functions *)
let find_uses_noncond funname funid file =
let rec iter_list biglist smalllist =
match smalllist with x::xs -> iter_list (remove_result biglist x) xs
                | _ -> biglist
in iter_list (find_uses funname funid file) (find_uses_cond funname funid file)

(* Finds calls of all functions in non-condition in all functions *)
let find_uses_noncond_all file =
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_uses_noncond "" dec.svar.vid file)@(iter_list xs)
                | _::xs -> iter_list xs
                | [] -> []
in iter_list file.globals

class find_calls_usesvar_with_tmp file result funname funid varname : nopCilVisitor =
object
inherit nopCilVisitor
method vinst instr = 
match instr with Call(lval_opt, Lval(Var(varinfo) ,_), arg_list, loc) -> if (is_equal_funname_funid varinfo funname funid)&&(List.length (List.flatten (List.map (fun x -> FuncVar.search_expression_list arg_list x loc (-1) true) (Hashtbl.find_all varnameMapping varname))) > 0) then (match lval_opt with Some((Var(tmpinfo), _)) -> if (String.length tmpinfo.vname > 2)&&(String.compare "tmp" (String.sub tmpinfo.vname 0 3) = 0) then result := (!result)@((tmpinfo.vid, varinfo.vid)::[]); SkipChildren
                                                                                | _ -> SkipChildren) else SkipChildren
        | _ -> SkipChildren
end

let find_lval_of_calls_usesvar funname funid varname file =
let result = ref []
in let visitor = new find_calls_usesvar_with_tmp file result funname funid varname
in visitCilFileSameGlobals visitor file; !result

(* Finds calls of a function with a variable as argument in conditions *)
let find_usesvar_cond funname funid varname file =
let id_list = find_lval_of_calls_usesvar funname funid varname file 
in let rec iter_list list = 
match list with (tmp, func)::xs -> (match FuncVar.find_uses_in_cond "" tmp file true with (name, loc, typ, id)::ys -> (create_fun_res "" func file loc)::(iter_list xs)
                                                                        | [] -> iter_list xs)
        | _ -> []
in iter_list id_list

(* Finds calls of all functions with a variable as argument in conditions *)
let find_usesvar_cond_all varname file =
let rec iter_list list =
match list with GFun(dec,_)::xs -> (find_usesvar_cond "" dec.svar.vid varname file)@(iter_list xs) 
        | _::xs -> iter_list xs
        | [] -> []
in iter_list file.globals

(* Finds calls of a function with a variable as argument in non-conditions *)
let find_usesvar_noncond funname funid varname file =
let rec iter_list biglist smalllist =
match smalllist with x::xs -> iter_list (remove_result biglist x) xs
                | _ -> biglist
in iter_list (find_usesvar funname funid varname file) (find_usesvar_cond funname funid varname file)

(* Finds calls of all functions with a variable as argument in non-conditions *)
let find_usesvar_noncond_all varname file =
let rec iter_list list =
match list with GFun(dec, _)::xs -> (find_usesvar_noncond "" dec.svar.vid varname file)@(iter_list xs)
        | _::xs -> iter_list xs
        | [] -> []
in iter_list file.globals