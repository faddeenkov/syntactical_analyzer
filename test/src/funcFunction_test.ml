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
                                         assert_bool "check result of third" (check_result (List.nth result 3) "" "void" 18));
"test find_def_all" >:: (fun _ -> let result = find_def_all (Frontc.parse "test6.c" ())
                                  in assert_equal (List.length result) 2;
                                     assert_bool "check result of first" (check_result (List.hd result) "f"  "void f (other_int x, float y)" 5);
                                     assert_bool "check result of second" (check_result (List.nth result 1) "main" "void main ()" 12));
"test find_uses" >:: (fun _ -> let result = find_uses "is_divisible" (-1) (Frontc.parse "test7.c" ())
                                 in assert_equal (List.length result) 2;
                                    assert_bool "check result of first" (check_result (List.hd result) "is_divisible" "int is_divisible (int a, int b)" 3);
                                    assert_bool "check result of second" (check_result (List.nth result 1) "is_divisible" "int is_divisible (int a, int b)" 9)) 

 ]