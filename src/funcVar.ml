open Cil

let rec find_var_in_varinfo_list list name = 
match list with x::xs -> if String.compare x.vname name = 0 then x.vid else find_var_in_varinfo_list xs name
            | [] -> -1

let find_uses_in_fun_var dec name = let id_local = find_var_in_varinfo_list dec.slocals name
in let id_formal = find_var_in_varinfo_list dec.sformals name
in 
let rec resolve_type t = 
match t with TInt(ikind, _) -> (match ikind with IChar -> "char"
                                                | ISChar -> "signed char"
                                                | IUChar -> "unsigned char"
                                                | IBool -> "bool"
                                                | IInt -> "int"
                                                | IUInt -> "unsigned int"
                                                | IShort -> "short"
                                                | IUShort -> "unsigned short"
                                                | ILong -> "long"
                                                | IULong -> "unsigned long"
                                                | ILongLong -> "long long"
                                                | IULongLong -> "unisgned long long")
        | TFloat(fkind, _) -> (match fkind with FFloat -> "float"
                                                | FDouble -> "double"
                                                | FLongDouble -> "long double"
                                                | FComplexFloat -> "float _Complex"
                                                | FComplexDouble -> "double _Complex"
                                                | FComplexLongDouble -> "long double _Complex")
        | TPtr(typ, _) -> "*"^(resolve_type typ)
        | TArray (typ, _, _) -> (resolve_type typ)^"[]"
        | TNamed (info, _) -> info.tname
        | TComp (info, _) -> info.cname
        | TEnum (info, _) -> info.ename
        | _ -> ""
in
let search_lhost host name loc = 
match host with Var(info) -> if String.compare info.vname name = 0 then (name, loc, (resolve_type info.vtype), info.vid)::[] else []
            | _ -> []
in
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