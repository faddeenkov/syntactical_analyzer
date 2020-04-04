open Cil
open Frontc

(*Input file*)
let filename = Sys.argv.(1)

(*Find global definition of x *)
let is_var_elem_global x elem  = match x with GVarDecl(var,loc) ->  if(String.compare elem var.vname = 0) then true else false
                                            | GVar(var, init, loc) -> if(String.compare elem var.vname = 0) then true else false
                                            | _ -> false
let rec find list elem acc = match list with x::xs -> if(is_var_elem_global x elem) then (find xs elem (x::acc)) else (find xs elem acc) | [] -> acc

(*Find x as lvalue in functions*)
let get_instr_list stm =  match stm.skind with Instr(list_instr) -> list_instr | _ -> [] 

let printout_if_elem_is_lvalue ins elem =  match ins with Set((host, _), exp, loc) -> (match host with Var(info)-> if (String.compare elem info.vname = 0) then Printf.printf "Found x as lvalue in line %d\n" loc.line | _ -> () )
                                         | _ -> ()

let rec iterate_over_instructions ins_list elem =  match ins_list with x::xs -> (printout_if_elem_is_lvalue x elem); (iterate_over_instructions xs elem) | [] -> ()

let rec iterate_over_stmts stmt_list elem =  match stmt_list with x::xs -> (iterate_over_instructions (get_instr_list x) elem); iterate_over_stmts xs elem | [] -> ()

let is_elem_in_fun_lvalue x elem = match x with GFun(dec, _) -> iterate_over_stmts dec.sbody.bstmts elem 
                                                | _ -> ()
let rec iterate_over_functions_searching_lval glob_list elem = match glob_list with x::xs -> is_elem_in_fun_lvalue x elem; iterate_over_functions_searching_lval xs elem | [] -> ()

(*Find x in a function call*)
let rec is_var_info_elem var elem loc =  if(String.compare elem var.vname = 0) then Printf.printf "x is an argument in a call of a function in line %d\n" loc.line else ()
                                                
let rec iterate_over_expr_funcall expr_list elem loc = match expr_list with x::xs -> (match x with Lval((Var(var), _)) -> is_var_info_elem var elem loc| _ -> ()) 
                                                                        | _ -> ()                                               
                                                                                          (*I dont know what the second argument is, I assume that the list are the parameters of a function call*)
let rec iterate_over_instr_funcall instr_list elem =  match instr_list with x::xs -> (match x with Call(_, expr, expr_list, loc) -> (iterate_over_expr_funcall expr_list elem loc); (iterate_over_instr_funcall xs elem)| _ -> iterate_over_instr_funcall xs elem)
                                                                        | [] -> ()
let rec iterate_over_stmts_funcall list elem = match list with x::xs -> (match x.skind with Instr(instr_list) -> iterate_over_instr_funcall instr_list elem | _ -> ())
                                                            | _ -> ()
let rec find_elem_in_funcall glob_list elem = match glob_list with x::xs -> (match x with GFun(dec,_)-> iterate_over_stmts_funcall dec.sbody.bstmts elem | _ -> ()); find_elem_in_funcall xs elem
                                                                | [] -> ()

(*Find x as a part of an expression when another variable is set*)
let is_elem_lval elem (host,o) = match host with Var(info) ->((String.compare elem info.vname) = 0) | _ -> false

let rec is_elem_in_expr elem expr = match expr with Lval(lval) -> is_elem_lval elem lval 
                                            | Real(e) -> is_elem_in_expr elem e 
                                            | Imag(e) -> is_elem_in_expr elem e
                                            | SizeOfE(e) -> is_elem_in_expr elem e
                                            | AlignOfE(e) -> is_elem_in_expr elem e
                                            | UnOp(u,e,t) -> is_elem_in_expr elem e
                                            | BinOp(b,e1,e2,t) -> (is_elem_in_expr elem e1)&&(is_elem_in_expr elem e2)
                                            | Question(e1,e2,e3,t) -> (is_elem_in_expr elem e1)&&(is_elem_in_expr elem e2)&&(is_elem_in_expr elem e3)
                                            | CastE(t,e) -> is_elem_in_expr elem e
                                            | AddrOf(lval) -> is_elem_lval elem lval
                                            | AddrOfLabel(stm_ref) -> Printf.printf "expression is address of label\n"; false
                                            | StartOf(lval) -> is_elem_lval elem lval
                                            | _ -> false
(*TODO ... but first refactor!!*)                                            

(*actual execution*)
let _ =       
let f =  Frontc.parse filename () in 
let elem_list = find f.globals "x" [] in
let rec found_elem_printout list = match list with t::xs -> (match t with GVarDecl(car, loc) -> Printf.printf "A global declaration of x in line: %d\n" loc.line
                                                            | GVar(car, init, loc) -> Printf.printf "A global definition of x in line: %d\n" loc.line
                                                            | _ -> Printf.printf "Error, found object could not be matched\n")
                                        | [] -> ()
in
found_elem_printout elem_list; iterate_over_functions_searching_lval f.globals "x"; find_elem_in_funcall f.globals "x"
                            
