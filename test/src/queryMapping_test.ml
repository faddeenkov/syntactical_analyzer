open OUnit2
open Mylib.QueryMapping
open Cil
open Mylib.JsonParser

let check_result res res_name res_typ res_line =
match res with (name, loc, typ, id) -> if (String.compare name res_name = 0)
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
                                   assert_bool "check result of second" (check_result (List.nth result 1) "increase_number" "int increase_number (int num)" 10))
] 