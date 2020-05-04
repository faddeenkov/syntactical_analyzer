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