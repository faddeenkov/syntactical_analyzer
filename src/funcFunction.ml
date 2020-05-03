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
