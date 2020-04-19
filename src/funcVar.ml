open Cil

let rec find_var_in_varinfo_list list name = 
match list with x::xs -> if String.compare x.vname name = 0 then x.vid else find_var_in_varinfo_list xs name
            | [] -> -1

let find_uses_in_fun_var dec name = let id_local = find_var_in_varinfo_list dec.slocals name
in let id_formal = find_var_in_varinfo_list dec.sformals name
in 
let search_lhost host name loc = Printf.printf "search_lhost was called\n";
match host with Var(info) -> if String.compare info.vname name = 0 then (name, loc, (Pretty.sprint 1 (d_type () info.vtype)), info.vid)::[] else []
            | _ -> []
in
(* let rec search_expression exp name loc = match exp with Lval((lhost, _)) -> search_lhost lhost name loc
                                                | Real(e) -> search_expression e name loc
in *)
let rec search_instr_list_for_var list name = 
match list with Set((lhost, offset), exp, loc)::xs ->  (search_lhost lhost name loc)@(search_instr_list_for_var xs name)
                | _::xs -> search_instr_list_for_var xs name
                | [] -> []
in 
let rec search_stmt_list_for_var list name = 
match list with x::xs -> (match x.skind with Instr(ins_list) -> search_instr_list_for_var ins_list name
                                            | _ -> [])
            | [] -> []
in search_stmt_list_for_var dec.sbody.bstmts name

let rec find_uses_in_fun_find_fun list name varname = 
match list with GFun(dec, loc)::xs -> if String.compare dec.svar.vname name = 0 then find_uses_in_fun_var dec varname else find_uses_in_fun_find_fun xs name varname
            | [] -> []
            | _::xs -> find_uses_in_fun_find_fun xs name varname

let find_uses_in_fun varname funname file = find_uses_in_fun_find_fun file.globals funname varname