open Cil

(* Helper functions *)
(* Finds a variable in a lhost *)
let search_lhost host name loc = 
match host with Var(info) -> if String.compare info.vname name = 0 then  (name, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::[]  else []
                (* Should I consider Mem too? *)
            | _ -> []

(* Finds a variable in an expression *)
let rec search_expression exp name loc = 
match exp with Lval((lhost, _)) -> search_lhost lhost name loc
        | Real(e) -> search_expression e name loc
        | Imag(e) -> search_expression e name loc
        | SizeOfE(e) -> search_expression e name loc
        | AlignOfE(e) -> search_expression e name loc
        | UnOp(_,e,_) -> search_expression e name loc
        | BinOp(_, e1, e2, _) -> (search_expression e1 name loc)@(search_expression e2 name loc)
        | Question(e1,e2,e3,_) -> (search_expression e1 name loc)@(search_expression e2 name loc)@(search_expression e3 name loc)
        | CastE(_,e) -> search_expression e name loc
        | AddrOf((lhost,_)) -> search_lhost lhost name loc
        | StartOf((lhost, _)) -> search_lhost lhost name loc
        | _ -> []
 
 (* Finds a variable in a list of expressions *)
let rec search_expression_list list name loc = 
match list with x::xs -> (search_expression x name loc)@(search_expression_list xs name loc)
                | [] -> []

(* Finds a variable in a list of instructions *)
let rec search_instr_list_for_var list name = 
match list with Set((lhost, offset), exp, loc)::xs ->  (search_lhost lhost name loc)@(search_expression exp name loc)@(search_instr_list_for_var xs name)
                | VarDecl(info, loc)::xs -> if (String.compare info.vname name = 0) then (name, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::(search_instr_list_for_var xs name) else (search_instr_list_for_var xs name)
                | Call (Some(lhost,_), exp, exp_list, loc)::xs -> (search_lhost lhost name loc)@(search_expression exp name loc)@(search_expression_list exp_list name loc)@(search_instr_list_for_var xs name)
                (* Should I consider Asm too? *)
                | _::xs -> search_instr_list_for_var xs name
                | [] -> []
 
 (* Finds a variable in a list of statements *)
let rec search_stmt_list_for_var list name = 
match list with x::xs -> (match x.skind with Instr(ins_list) -> search_instr_list_for_var ins_list name
                                            | Return(Some(exp), loc) -> search_expression exp name loc
                                            | Goto(s_ref, loc) -> search_stmt_list_for_var ((!s_ref)::[]) name
                                            | ComputedGoto(exp, loc) -> search_expression exp name loc
                                            | If (exp, b1, b2, loc) -> (search_expression exp name loc)@(search_stmt_list_for_var b1.bstmts name)@(search_stmt_list_for_var b2.bstmts name) 
                                            | Switch (exp, block, stmt_list, loc) -> (search_expression exp name loc)@(search_stmt_list_for_var stmt_list name) (* (search_stmt_list_for_var block.bstmts name)@ is this a duplicate of stmt list? *)
                                            | Loop (block, loc, None, None) -> search_stmt_list_for_var block.bstmts name
                                            | Loop (block, loc, None, Some(s2)) -> (search_stmt_list_for_var block.bstmts name)@(search_stmt_list_for_var (s2::[]) name)
                                            | Loop (block, loc, Some(s1), None) -> (search_stmt_list_for_var block.bstmts name)@(search_stmt_list_for_var (s1::[]) name)
                                            | Loop (block, loc, Some(s1), Some(s2)) -> (search_stmt_list_for_var block.bstmts name)@(search_stmt_list_for_var (s1::[]) name)@(search_stmt_list_for_var (s2::[]) name)
                                            | Block(block) -> Printf.printf "A Block\n"; search_stmt_list_for_var block.bstmts name
                                            | TryFinally (b1, b2, loc) -> (search_stmt_list_for_var b1.bstmts name)@(search_stmt_list_for_var b2.bstmts name)
                                            | TryExcept(b1, (instr_list, exp), b2, loc) -> (search_stmt_list_for_var b1.bstmts name)@(search_instr_list_for_var instr_list name)@(search_expression exp name loc)@(search_stmt_list_for_var b2.bstmts name)
                                            | _ -> [])@(search_stmt_list_for_var xs name)
            | [] -> []

(* Finds all uses of a variable in a function-body *)
let find_uses_in_fun_var dec name = search_stmt_list_for_var dec.sbody.bstmts name

(* Finds the function in which a variable shall be found *)
let rec find_uses_in_fun_find_fun list name varname = 
match list with GFun(dec, loc)::xs -> if String.compare dec.svar.vname name = 0 then find_uses_in_fun_var dec varname else find_uses_in_fun_find_fun xs name varname
            | [] -> []
            | _::xs -> find_uses_in_fun_find_fun xs name varname

(* Finds all uses of a variable in a function *)
let find_uses_in_fun varname funname file = find_uses_in_fun_find_fun file.globals funname varname