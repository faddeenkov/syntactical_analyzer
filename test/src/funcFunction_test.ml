open OUnit2
open Cil
open SyntacticalAnalyzer.FuncFunction

let check_result res res_name res_typ res_line =
match res with (name, loc, typ, id) -> if (String.compare name res_name = 0)
                                      &&(String.compare typ res_typ = 0)
                                      &&(loc.line == res_line) then true else false

let rec print_result list =
match list with (name, loc, kind, id)::xs -> Printf.printf "name:%s, loc.line=%d, loc.file=%s, loc.byte:%d, kind:%s, id:%d \n" name loc.line loc.file loc.byte kind id; print_result xs
            | [] -> ()

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
                                    assert_bool "check result of first" (check_result (List.hd result) "is_divisible" "int is_divisible (int a, int b)" 4);
                                    assert_bool "check result of second" (check_result (List.nth result 1) "is_divisible" "int is_divisible (int a, int b)" 10));
"test find_uses_in_fun_all" >:: (fun _ -> let result = find_uses_in_fun_all "too_complicated_calculation" (Frontc.parse "test8.c" ())
                                          in assert_equal (List.length result) 2;
                                             assert_bool "check result of first" (check_result (List.hd result) "double_number" "int double_number (int num)" 10);
                                             assert_bool "check result of second" (check_result (List.nth result 1) "increase_number" "int increase_number (int num)" 10));
"test find_usesvar_in_fun" >:: (fun _ -> let result = find_usesvar_in_fun "f" (-1) "main" "num2" (Frontc.parse "test9.c" ())
                                         in assert_equal (List.length result) 2;
                                            assert_bool "check result of first" (check_result (List.hd result) "f" "int f (int a)" 11);
                                            assert_bool "check result of second" (check_result (List.nth result 1) "f" "int f (int a)" 15));
"test find_usesvar_all" >:: (fun _ -> let result = find_usesvar_all "z" (Frontc.parse "test10.c" ())
                                      in assert_equal (List.length result) 3;
                                         assert_bool "check result of first" (check_result (List.hd result) "f" "int f (int a, int b)" 10);
                                         assert_bool "check result of second" (check_result (List.nth result 1) "g" "int g (int b)" 14);
                                         assert_bool "check result of third" (check_result (List.nth result 2) "h" "void h (double x, int z)" 15));
"test find_uses_cond" >:: (fun _ -> let result = find_uses_cond "f" (-1) (Frontc.parse "test11.c" ())
                                    in assert_equal (List.length result) 5;
                                       assert_bool "check result of first" (check_result (List.hd result) "f" "int f (int a)" 2);
                                       assert_bool "check result of second" (check_result (List.nth result 1) "f" "int f (int a)" 5);
                                       assert_bool "check result of third" (check_result (List.nth result 2) "f" "int f (int a)" 13);
                                       assert_bool "check result of fourth" (check_result (List.nth result 3) "f" "int f (int a)" 17);
                                       assert_bool "check result of fifth" (check_result (List.nth result 4) "f" "int f (int a)" 24););
"test find_uses_noncond_all" >:: (fun _ -> let result = find_uses_noncond_all (Frontc.parse "test10.c" ())
                                           in assert_equal (List.length result) 2;
                                              assert_bool "check result of first" (check_result (List.hd result) "f" "int f (int a, int b)" 10);
                                              assert_bool "check result of second" (check_result (List.nth result 1) "h" "void h (double x, int z)" 15));
"test find_usesvar_cond_all" >:: (fun _ -> let result = find_usesvar_cond_all "a" (Frontc.parse "test12.c" ())
                                           in assert_equal (List.length result) 2;
                                              assert_bool "check result of first" (check_result (List.hd result) "f" "int f (int x)" 7);
                                              assert_bool "check result of second" (check_result (List.nth result 1) "g" "int g (int a, int b)" 15));
"test find_usesvar_noncond_all" >:: (fun _ -> let result = find_usesvar_noncond_all "z" (Frontc.parse "test10.c" ())
                                              in assert_equal (List.length result) 2;
                                                 assert_bool "check result of first" (check_result (List.hd result) "f" "int f (int a, int b)" 10);
                                                 assert_bool "check result of second" (check_result (List.nth result 1) "h" "void h (double x, int z)" 15));
"test find_usesvar so that it finds CIL-renamed vars too" >:: (fun _ -> let result = find_usesvar "f" (-1) "i" (Frontc.parse "test16.c" ())
                                                               in 
                                                                  assert_bool "check result of first" (check_result (List.hd result) "f" "void f (int x)" 14);
                                                                  assert_bool "check result of second" (check_result (List.nth result 1) "f" "void f (int x)" 9);
                                                                  assert_bool "check result of third" (check_result (List.nth result 2) "f" "void f (int x)" 11)) 

 ]