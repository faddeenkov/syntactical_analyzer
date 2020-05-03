open OUnit2
open Cil
open Mylib.FuncFunction

let check_result res res_name res_typ res_line =
match res with (name, loc, typ, id) -> if (String.compare name res_name = 0)
                                      &&(String.compare typ res_typ = 0)
                                      &&(loc.line == res_line) then true else false

let funcfunction_tests = "test suite for func_Function" >::: [
"test find_returns_all" >:: (fun _ -> let result = find_returns_all (Frontc.parse "test2.c" ())
                                      in assert_equal (List.length result) 4;
                                         (* Here is an implicit return-statement in function f in test2.c *)
                                         assert_bool "check result of first" (check_result (List.nth result 1) "" "int" 9);
                                         assert_bool "check result of second" (check_result (List.nth result 2) "" "int" 12);
                                         assert_bool "check result of third" (check_result (List.nth result 3) "" "void" 18))
]