open Cil

(* Helper functions *)
let is_equal_varname_varid varinfo name id = if ((String.compare varinfo.vname name = 0) || (varinfo.vid = id)) then true else false

(* Finds a variable in a lhost *)
let search_lhost host name loc varid = 
match host with Var(info) -> if is_equal_varname_varid info name varid then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::[]  else []
                (* Should I consider Mem too? *)
            | _ -> []

(* Finds a variable in an expression *)
let rec search_expression exp name loc varid = 
match exp with Lval((lhost, _)) -> search_lhost lhost name loc varid
        | Real(e) -> search_expression e name loc varid
        | Imag(e) -> search_expression e name loc varid
        | SizeOfE(e) -> search_expression e name loc varid
        | AlignOfE(e) -> search_expression e name loc varid
        | UnOp(_,e,_) -> search_expression e name loc varid
        | BinOp(_, e1, e2, _) -> (search_expression e1 name loc varid)@(search_expression e2 name loc varid)
        | Question(e1,e2,e3,_) -> (search_expression e1 name loc varid)@(search_expression e2 name loc varid)@(search_expression e3 name loc varid)
        | CastE(_,e) -> search_expression e name loc varid
        | AddrOf((lhost,_)) -> search_lhost lhost name loc varid
        | StartOf((lhost, _)) -> search_lhost lhost name loc varid
        | _ -> []
 
 (* Finds a variable in a list of expressions *)
let rec search_expression_list list name loc varid = 
match list with x::xs -> (search_expression x name loc varid)@(search_expression_list xs name loc varid)
                | [] -> []

(* Finds a variable in a list of instructions *)
let rec search_instr_list_for_var list name varid = 
match list with Set((lhost, offset), exp, loc)::xs ->  (search_lhost lhost name loc varid)@(search_expression exp name loc varid)@(search_instr_list_for_var xs name varid)
                | VarDecl(info, loc)::xs -> if is_equal_varname_varid info name varid then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::(search_instr_list_for_var xs name varid) else (search_instr_list_for_var xs name varid)
                | Call (Some(lhost,_), exp, exp_list, loc)::xs -> (search_lhost lhost name loc varid)@(search_expression exp name loc varid)@(search_expression_list exp_list name loc varid)@(search_instr_list_for_var xs name varid)
                (* Should I consider Asm too? *)
                | _::xs -> search_instr_list_for_var xs name varid
                | [] -> []
 
 (* Finds a variable in a list of statements *)
let rec search_stmt_list_for_var list name varid = 
match list with x::xs -> (match x.skind with Instr(ins_list) -> search_instr_list_for_var ins_list name varid
                                            | Return(Some(exp), loc) -> search_expression exp name loc varid
                                            | Goto(s_ref, loc) -> search_stmt_list_for_var ((!s_ref)::[]) name varid
                                            | ComputedGoto(exp, loc) -> search_expression exp name loc varid
                                            | If (exp, b1, b2, loc) -> (search_expression exp name loc varid)@(search_stmt_list_for_var b1.bstmts name varid)@(search_stmt_list_for_var b2.bstmts name varid) 
                                            | Switch (exp, block, stmt_list, loc) -> (search_expression exp name loc varid)@(search_stmt_list_for_var stmt_list name varid) (* (search_stmt_list_for_var block.bstmts name)@ is this a duplicate of stmt list? *)
                                            | Loop (block, loc, None, None) -> search_stmt_list_for_var block.bstmts name varid
                                            | Loop (block, loc, None, Some(s2)) -> (search_stmt_list_for_var block.bstmts name varid)@(search_stmt_list_for_var (s2::[]) name varid)
                                            | Loop (block, loc, Some(s1), None) -> (search_stmt_list_for_var block.bstmts name varid)@(search_stmt_list_for_var (s1::[]) name varid)
                                            | Loop (block, loc, Some(s1), Some(s2)) -> (search_stmt_list_for_var block.bstmts name varid)@(search_stmt_list_for_var (s1::[]) name varid)@(search_stmt_list_for_var (s2::[]) name varid)
                                            | Block(block) -> Printf.printf "A Block\n"; search_stmt_list_for_var block.bstmts name varid
                                            | TryFinally (b1, b2, loc) -> (search_stmt_list_for_var b1.bstmts name varid)@(search_stmt_list_for_var b2.bstmts name varid)
                                            | TryExcept(b1, (instr_list, exp), b2, loc) -> (search_stmt_list_for_var b1.bstmts name varid)@(search_instr_list_for_var instr_list name varid)@(search_expression exp name loc varid)@(search_stmt_list_for_var b2.bstmts name varid)
                                            | _ -> [])@(search_stmt_list_for_var xs name varid)
            | [] -> []

(* Finds all uses of a variable in a function-body *)
let find_uses_in_fun_var dec name varid = search_stmt_list_for_var dec.sbody.bstmts name varid

(* Finds the function in which a variable shall be found *)
let rec find_uses_in_fun_find_fun list name varname varid = 
match list with GFun(dec, loc)::xs -> if String.compare dec.svar.vname name = 0 then find_uses_in_fun_var dec varname varid else find_uses_in_fun_find_fun xs name varname varid
            | [] -> []
            | _::xs -> find_uses_in_fun_find_fun xs name varname varid
(*
class var_search_in_fun_visitor varname funname varid : nopCilVisitor = 
    object(self)
        inherit nopCilVisitor
        val mutable result = []
        method vinst instr = result <- result@(search_instr_list_for_var (instr::[]) varname varid); SkipChildren
        method vstmt stmt = DoChildren
        method vblock block = DoChildren
        method vfunc dec = if (String.compare dec.svar.vname funname = 0) then DoChildren else SkipChildren
        method vglob glob = match glob with GFun(_) -> DoChildren
                                | _ -> SkipChildren
        method get_result = result
    end

let find_uses_in_fun varname varid funname file = 
let visitor = new var_search_in_fun_visitor varname funname varid
in visitCilFileSameGlobals visitor file; visitor#get_result *)


(* Finds all uses of a variable in a function *)
let find_uses_in_fun varname varid funname file = find_uses_in_fun_find_fun file.globals funname varname varid 

let rec find_all_glob_vars list = 
match list with GVar(info, _, _)::xs -> info.vid::(find_all_glob_vars xs)
            | _::xs -> find_all_glob_vars xs
            | [] -> []

(* Finds all uses of all global variables in a function *)
let find_uses_in_fun_all_glob funname file = 
let id_list = find_all_glob_vars file.globals
in let rec iter_list l = 
match l with x::xs -> (find_uses_in_fun "" x funname file)@(iter_list xs)
            | [] -> []
in iter_list id_list

let rec find_fundec globals funname = 
match globals with GFun(dec, _)::xs -> if (String.compare dec.svar.vname funname = 0) then Some(dec) else find_fundec xs funname
                | _::xs -> find_fundec xs funname
                | [] -> None

(* Finds all uses of all variables in a function *)
let find_uses_in_fun_all funname file = 
let fundec_opt = find_fundec file.globals funname
in let rec iter_list l = 
match l with x::xs -> (find_uses_in_fun "" x funname file)@(iter_list xs)
            | [] -> []
in
match fundec_opt with None -> []
                | Some(fundec) -> (find_uses_in_fun_all_glob funname file)@(iter_list (List.map (fun x -> x.vid) fundec.sformals))@(iter_list (List.map (fun x -> x.vid) fundec.slocals))