open Cil

let is_equal_funname_funid varinfo name id = if ((String.compare varinfo.vname name = 0) || (varinfo.vid = id)) then true else false

class fun_find_returns funname funid result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vglob global = DoChildren
method vfunc fundec = if is_equal_funname_funid fundec.svar funname funid then DoChildren else SkipChildren
method vblock block = DoChildren
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
method vglob global = DoChildren
method vfunc fundec = DoChildren
method vblock block = DoChildren
method vstmt stmt = DoChildren
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
method vglob global = DoChildren
method vfunc fundec = if is_equal_funname_funid fundec.svar funstrucname (-1) then DoChildren else SkipChildren
method vblock block = DoChildren
method vstmt stmt = DoChildren
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
method vglob global = DoChildren
method vfunc dec = if is_equal_funname_funid dec.svar funstrucname (-1) then DoChildren else SkipChildren
method vblock block = DoChildren
method vstmt stmt = DoChildren
method vinst instr =
match instr with Call(_, exp, list, loc) -> (match exp with Lval(Var(varinfo), _) -> if is_equal_funname_funid varinfo fundec.svar.vname fundec.svar.vid then (if List.length (FuncVar.search_expression_list list varname loc_default varid) > 0 then (result := (!result)@((varinfo.vname, loc, create_sig fundec file, varinfo.vid)::[]); SkipChildren) else SkipChildren) else SkipChildren
                                                        | _ -> Printf.printf "some other exp in call\n"; SkipChildren)
                 | _ -> SkipChildren
end 

(* Finds calls of a function with a var in argument in a function *)
let find_usesvar_in_fun funname funid funstrucname varname file =
let fundec_opt = find_fundec funname funid file.globals
in let result = ref []
in match fundec_opt with None -> []
| Some(fundec) ->  ignore(visitCilFileSameGlobals (new fun_find_usesvar_in_fun fundec funstrucname varname (-1) file result) file); !result

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









(* Following part is not ready *)
(* Generates ID-list of all tmp__-vars in funstrucname *)
class fun_find_cil_gen_tmp funname funid funstrucname file result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vglob global = DoChildren
method vfunc fundec = if is_equal_funname_funid fundec.svar funstrucname (-1) then DoChildren else SkipChildren
method vblock block = DoChildren
method vstmt stmt = DoChildren
method vinst instr = 
match instr with Call(Some((Var(tmpinfo), _)), Lval(Var(varinfo) ,_), list,loc) -> 
if is_equal_funname_funid varinfo funname funid then (if (String.length tmpinfo.vname > 4)&&(String.compare "tmp__" (String.sub tmpinfo.vname 0 5) = 0) then (result := (!result)@((tmpinfo.vid, loc, varinfo.vname, varinfo.vid)::[]); SkipChildren) else SkipChildren)  else SkipChildren
            | Call(_,_, _, _) -> Printf.printf "Some other call\n"; SkipChildren
            | _ -> SkipChildren
end
(* Output: tmp__-ID, funloc, funname, funid *)

let rec printout_idlist list =
match list with (tmpid, funloc, funname, funid)::xs -> (Printf.printf "tmpid:%i line: %i funname: %s funid: %i\n" tmpid funloc.line funname funid); printout_idlist xs
                | [] -> ()

(* Finds calls of a function in conditions in a function DOES NOT WORK YET *)
let find_uses_in_cond_in_fun funname funid funstrucname file =
let result = ref []
in let visitor = new fun_find_cil_gen_tmp funname funid funstrucname file result
in let id_list = ignore (visitCilFileSameGlobals visitor file); !result
in let rec iter_ids list fundec_struc fundec_target funname funid =  Printf.printf "list.length: %i\n" (List.length list);
match list with x::xs -> (match FuncVar.cond_search_uses_stmt_list fundec_struc.sbody.bstmts "" x with [] -> Printf.printf "no result\n"; iter_ids xs fundec_struc fundec_target funname funid
                                                                                            | (name, loc, typ, id)::ys -> Printf.printf "result of cond-search\n"; (funname, loc, create_sig fundec_target file, funid)::(iter_ids xs fundec_struc fundec_target funname funid))
        | [] -> []
in let fundec_struc_target = (find_fundec funstrucname (-1) file.globals, find_fundec funname funid file.globals)
in printout_idlist id_list; match fundec_struc_target with (Some(struc_fundec), Some(target_fundec)) ->  (match id_list with (_,_, real_funname, real_funid)::_ -> iter_ids (List.map (fun (tmpid, _,_,_) -> tmpid) id_list) struc_fundec target_fundec real_funname real_funid
                                                                                                | [] -> [])
                            | _ -> []
