open OUnit2
open SyntacticalAnalyzer.QueryMapping
open Cil
open SyntacticalAnalyzer.JsonParser

let check_result res res_name res_typ res_line =
match res with (name, loc, typ, _) -> if (String.compare name res_name = 0)
                                      &&(String.compare typ res_typ = 0)
                                      &&(loc.line == res_line) then true else false

let mapping_tests = "test suite for queryMapper" >::: [
"test and-target var_uses" >:: (fun _ -> let result = map_query (parse_json_file "test5.json") (Frontc.parse "test3.c" ())
                                 in assert_equal (List.length result) 2;
                                    assert_bool "check result of first" (check_result (List.hd result) "copy_var" "struct var" 13);
                                    assert_bool "check result of second" (check_result (List.nth result 1) "new_var" "struct var" 13) );
"test and-target fun_uses" >:: (fun _ -> let result = map_query (parse_json_file "test6.json") (Frontc.parse "test8.c" ())
                                in assert_equal (List.length result) 2;
                                   assert_bool "check result of first" (check_result (List.hd result) "double_number" "int double_number (int num)" 10);
                                   assert_bool "check result of second" (check_result (List.nth result 1) "increase_number" "int increase_number (int num)" 10));
"test and-target var_uses_in_fun" >:: (fun _ -> let result = map_query (parse_json_file "test7.json") (Frontc.parse "test13.c" ())
                                       in assert_equal (List.length result) 4;
                                          assert_bool "check result of first" (check_result (List.hd result) "a" "int" 17);
                                          assert_bool "check result of second" (check_result (List.nth result 1) "b" "int" 17);
                                          assert_bool "check result of third" (check_result (List.nth result 2) "a" "int" 18);
                                          assert_bool "check result of fourth" (check_result (List.nth result 3) "b" "int" 18));
"test or-target datatype_defs" >:: (fun _ -> let result = map_query (parse_json_file "test8.json") (Frontc.parse "test14.c" ())
                                             in assert_equal (List.length result) 3;
                                                assert_bool "check result of first" (check_result (List.hd result) "" "student" 1);
                                                assert_bool "check result of second" (check_result (List.nth result 1) "" "SomeUnion" 12);
                                                assert_bool "check result of third" (check_result (List.nth result 2) "" "State" 10));
"test or-target fun_returns" >:: (fun _ -> let result = map_query (parse_json_file "test9.json") (Frontc.parse "test2.c" ())
                                           in assert_equal (List.length result) 3;
                                              assert_bool "check result of first" (check_result (List.hd result) "" "int" 9);
                                              assert_bool "check result of second" (check_result (List.nth result 1) "" "int" 12);
                                              assert_bool "check result of third" (check_result (List.nth result 2) "" "void" 18))
]
