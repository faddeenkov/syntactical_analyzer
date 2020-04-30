open Cil

(* Helper functions *)
let is_equal_varname_varid varinfo name id = if ((String.compare varinfo.vname name = 0) || (varinfo.vid = id)) then true else false

class var_search_in_expr varname varid loc result : nopCilVisitor =
object(self)
inherit nopCilVisitor
method vvrbl info = if (is_equal_varname_varid info varname varid) then (result := (!result)@((info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::[]); SkipChildren) else SkipChildren
method vlval (h,o) = DoChildren
method vexpr exp = DoChildren
end

(* Finds a variable in an expression *)
let search_expression exp name loc varid = 
let result = ref []
in let visitor = new var_search_in_expr name varid loc result
in ignore (visitCilExpr visitor exp); !result

(* Finds a variable in a lhost *)
let search_lhost host name loc varid = 
match host with Var(info) -> if is_equal_varname_varid info name varid then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::[] else []
                (* Should I consider Mem too? *)
            | Mem(exp) -> Printf.printf "It's a pointer\n"; search_expression exp name loc varid
 
let rec search_offset os name loc varid =
match os with NoOffset -> []
            | Field(_, offset) -> search_offset offset name loc varid 
            | Index(exp, offset) -> (search_expression exp name loc varid)@(search_offset offset name loc varid)

(* Finds a variable in a list of expressions *)
let rec search_expression_list list name loc varid = 
match list with x::xs -> (search_expression x name loc varid)@(search_expression_list xs name loc varid)
                | [] -> []

(* Finds a variable in a list of instructions *)
let rec search_instr_list_for_var list name varid = 
match list with Set((lhost, offset), exp, loc)::xs ->  (search_lhost lhost name loc varid)@(search_offset offset name loc varid)@(search_expression exp name loc varid)@(search_instr_list_for_var xs name varid)
                | VarDecl(info, loc)::xs -> if is_equal_varname_varid info name varid then (info.vname, loc, (String.trim (Pretty.sprint 1 (d_type () info.vtype))), info.vid)::(search_instr_list_for_var xs name varid) else (search_instr_list_for_var xs name varid)
                | Call (Some(lhost,offset), exp, exp_list, loc)::xs -> (search_lhost lhost name loc varid)@(search_offset offset name loc varid)@(search_expression exp name loc varid)@(search_expression_list exp_list name loc varid)@(search_instr_list_for_var xs name varid)
                | Call (None, exp, exp_list, loc)::xs -> (search_expression exp name loc varid)@(search_expression_list exp_list name loc varid)@(search_instr_list_for_var xs name varid)
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

(* Find all uses of a variable in all functions *)
let find_uses varname varid file = 
let rec find_uses_in_all_fun l = 
match l with GFun(dec, _)::xs -> (find_uses_in_fun varname varid dec.svar.vname file)@(find_uses_in_all_fun xs)
            | _ ::xs -> find_uses_in_all_fun xs
            | [] -> []
in find_uses_in_all_fun file.globals

(* Finds all uses of global variables in all functions *)
let find_uses_all_glob file =
let rec iter_functions list = 
match list with GFun(dec,_)::xs -> (find_uses_in_fun_all_glob dec.svar.vname file)@(iter_functions xs)
            | _ ::xs -> iter_functions xs
            | [] -> []
in iter_functions file.globals

(* Finds uses of all variables in all functions *)
let find_uses_all file =
let rec iter_functions list = 
match list with GFun(dec,_)::xs -> (find_uses_in_fun_all dec.svar.vname file)@(iter_functions xs)
            | _ ::xs -> iter_functions xs
            | [] -> []
in iter_functions file.globals

let rec cond_search_uses_stmt_list list varname varid =
match list with x::xs -> (match x.skind with If(exp, b1, b2, loc) -> (search_expression exp varname loc varid)@(cond_search_uses_stmt_list (b1.bstmts@b2.bstmts) varname varid)
                                        | Switch(exp, block, stmt_list, loc) -> (search_expression exp varname loc varid)@(cond_search_uses_stmt_list block.bstmts varname varid)
                                        | Loop(block, loc, None, None) -> (cond_search_uses_stmt_list block.bstmts varname varid)
                                        | Loop (block, loc, None, Some(s1)) -> (cond_search_uses_stmt_list (s1::block.bstmts) varname varid)
                                        | Loop(block, loc, Some(s2), None) -> (cond_search_uses_stmt_list (s2::block.bstmts) varname varid)
                                        | Loop (block, loc, Some(s2), Some(s1)) -> (cond_search_uses_stmt_list (s2::s1::block.bstmts) varname varid)
                                        | Block(block) -> (cond_search_uses_stmt_list block.bstmts varname varid)
                                        | TryFinally (b1, b2, loc) -> (cond_search_uses_stmt_list (b1.bstmts@b2.bstmts) varname varid)
                                        | TryExcept(b1, _, b2, loc) -> (cond_search_uses_stmt_list (b1.bstmts@b2.bstmts) varname varid)
                                        | _ -> [] )@(cond_search_uses_stmt_list xs varname varid)
            | [] -> []

(* Finds all uses of a variable in conditions of a function *)
let find_uses_in_cond_in_fun varname varid funname file =
let fundec_opt = find_fundec file.globals funname
in match fundec_opt with None -> []
| Some(fundec) -> cond_search_uses_stmt_list fundec.sbody.bstmts varname varid

(* Finds all uses of a variable in conditions in all functions *)
let find_uses_in_cond varname varid file = 
let rec iter_functions list = 
match list with GFun(dec,_)::xs -> (cond_search_uses_stmt_list dec.sbody.bstmts varname varid)@(iter_functions xs)
            | _ ::xs -> iter_functions xs
            | [] -> []
in iter_functions file.globals