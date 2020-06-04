open OUnit2
open Mylib.FuncVar
open Cil

let location1 = {line = 1;file = "test";byte = 0}
let location2 = {line = 2;file = "test";byte = 5}
let location3 = {line = 3;file = "test";byte = 8}

let varinfo1 = {vname = "x"; 
                  vtype = TInt(IInt,[]); 
                  vattr = [];
                  vstorage = NoStorage;
                  vglob = true;
                  vinline = false;
                  vdecl = location1;
                  vinit = {init = None};
                  vid = 0;
                  vaddrof = false;
                  vreferenced = true;
                  vdescr = Pretty.nil;
                  vdescrpure = true;
                  vhasdeclinstruction = true}

let lhost1 = Var(varinfo1)

let varinfo2 = {vname = "y"; 
                  vtype = TInt(IInt,[]); 
                  vattr = [];
                  vstorage = NoStorage;
                  vglob = true;
                  vinline = false;
                  vdecl = location1;
                  vinit = {init = None};
                  vid = 1;
                  vaddrof = false;
                  vreferenced = true;
                  vdescr = Pretty.nil;
                  vdescrpure = true;
                  vhasdeclinstruction = true}

let lhost2 = Var(varinfo2)

let result1 = ("x", location1, "int", 0)::[]
let result2 = ("y", location2, "int", 1)::[]
let result3 = ("x", location3, "int", 0)::[]
let result4 = result1@result1
let result5 = result4@result3

let expression1 = Lval(lhost1, NoOffset)
let expression2 = AddrOf(lhost2, NoOffset)
let expression3 = BinOp(PlusA, expression1, expression2, TInt(IInt,[]))
let expr_list = expression1::expression3::[]

let instruction1 = Set((lhost1, NoOffset), expression2, location1)
let instruction2 = VarDecl(varinfo1, location1)
let instr_list = instruction1::instruction2::[]

let statement1 = {labels = [];
                  skind = Instr(instr_list);
                  sid = 3;
                  succs = [];
                  preds = []}

let statement2 = {labels = [];
                  skind = Return(None, location1);
                  sid = 4;
                  succs = [];
                  preds = []}

let statement3 = {labels = [];
                  skind = ComputedGoto(expression1, location3);
                  sid = 5;
                  succs = [];
                  preds = []}

let stmt_list = statement1::statement2::statement3::[]

let check_result res res_name res_typ res_line =
match res with (name, loc, typ, id) -> if (String.compare name res_name = 0)
                                      &&(String.compare typ res_typ = 0)
                                      &&(loc.line == res_line) then true else false

let rec print_out result = match result with (name, loc, _, _)::xs -> Printf.printf "name: %s line:%i\n" name loc.line; print_out xs
| [] -> ()

let funcvar_tests = "test suite for func_Var" >::: [
  (* "search lhost1"  >:: (fun _ -> assert_equal (search_lhost lhost1 "x" location1 (-1)) result1);
  "search lhost1 by id" >:: (fun _ -> assert_equal (search_lhost lhost1 "" location1 0) result1);
  "search expression1" >:: (fun _ -> assert_equal (search_expression expression1 "x" location1 (-1)) result1);
  "search expression2" >:: (fun _ -> assert_equal (search_expression expression3 "y" location2 (-1)) result2);
  "search expression-list1" >:: (fun _ -> assert_equal (search_expression_list expr_list "x" location1 (-1)) result4);
  "search instruction-list1" >:: (fun _ -> assert_equal (search_instr_list_for_var instr_list "x" (-1)) result4);
  "search statement-list1" >:: (fun _ -> assert_equal (search_stmt_list_for_var stmt_list "x" (-1)) result5); *)
  "test find_uses_in_fun_all_glob" >:: (fun _ -> let result = find_uses_in_fun_all_glob "main" (Frontc.parse "test.c" ()) false
                                                  in assert_equal (List.length result) 3;
                                                     assert_bool "check result of first" (check_result (List.hd result) "x" "int" 7);
                                                     assert_bool "check result of second" (check_result (List.nth result 1) "x" "int" 10);
                                                     assert_bool "check result of third" (check_result (List.nth result 2) "y" "char" 8));
  "test find_uses_in_fun_all" >:: (fun _ -> let result = find_uses_in_fun_all "niceFunction" (Frontc.parse "test1.c" ()) false
                                            in assert_equal (List.length result) 4;
                                               assert_bool "check result of first" (check_result (List.hd result) "global" "int" 5);
                                               assert_bool "check result of second" (check_result (List.nth result 1) "formal" "float" 5);
                                               assert_bool "check result of third" (check_result (List.nth result 2) "local" "float" 4);
                                               assert_bool "check result of fourth" (check_result (List.nth result 3) "local" "float" 5));
  "test find_uses" >:: (fun _ -> let result = find_uses "global" (-1) (Frontc.parse "test2.c" ()) false
                                 in assert_equal (List.length result) 4;
                                    assert_bool "check result of first" (check_result (List.hd result) "global" "char" 1);
                                    assert_bool "check result of second" (check_result (List.nth result 1) "global" "char" 4);
                                    assert_bool "check result of third" (check_result (List.nth result 2) "global" "char" 8);
                                    assert_bool "check result of fourth" (check_result (List.nth result 3) "global" "char" 17));
   "test find_uses_all" >:: (fun _ -> let result = find_uses_all (Frontc.parse "test3.c" ()) false
                                      in assert_equal (List.length result) 5;
                                         assert_bool "check result of first" (check_result (List.hd result) "new_var" "struct var" 5);
                                         assert_bool "check result of second" (check_result (List.nth result 1) "new_var" "struct var" 8);
                                         assert_bool "check result of third" (check_result (List.nth result 2) "new_var" "struct var" 8);
                                         assert_bool "check result of fourth" (check_result (List.nth result 3) "new_var" "struct var" 13);
                                         assert_bool "check result of fifth" (check_result (List.nth result 4) "copy_var" "struct var" 13));    
   "test find_uses_in_cond" >:: (fun _ -> let result = find_uses_in_cond "i" (-1) (Frontc.parse "test4.c" ()) false
                                          in assert_equal (List.length result) 3;
                                             assert_bool "check result of first" (check_result (List.hd result) "i" "int" 4);
                                             assert_bool "check result of second" (check_result (List.nth result 1) "i" "int" 5);
                                             assert_bool "check result of third" (check_result (List.nth result 2) "i" "int" 10));
   "test find_uses_in_cond_all" >:: (fun _ -> let result = find_uses_in_cond_all (Frontc.parse "test5.c" ()) false
                                              in assert_equal (List.length result) 4;
                                                 assert_bool "check result of first" (check_result (List.hd result) "global" "int" 5);
                                                 assert_bool "check result of second" (check_result (List.nth result 1) "j" "int" 5);
                                                 assert_bool "check result of third" (check_result (List.nth result 2) "global" "int" 16);
                                                 assert_bool "check result of fourth" (check_result (List.nth result 3) "x" "int" 11));
   "test find_uses_in_noncond_all" >:: (fun _ -> let result = find_uses_in_noncond_all (Frontc.parse "test5.c" ()) false
                                                 in assert_equal (List.length result) 7;
                                                    assert_bool "check result of first" (check_result (List.hd result) "global" "int" 1);
                                                    assert_bool "check result of second" (check_result (List.nth result 1) "j" "int" 4);
                                                    assert_bool "check resut of third" (check_result (List.nth result 2) "global" "int" 13);
                                                    assert_bool "check result of fourth" (check_result (List.nth result 4) "global" "int" 17);
                                                    assert_bool "check result of fifth" (check_result (List.nth result 6) "x" "int" 17)); 
   "test find_decl_all" >:: (fun _ -> let result = find_decl_all (Frontc.parse "test5.c" ())
                                       in assert_equal (List.length result) 3;
                                          assert_bool "check result of first" (check_result (List.hd result) "global" "int" 1);
                                          assert_bool "check result of second" (check_result (List.nth result 1) "j" "int" 4);
                                          assert_bool "check result of third" (check_result (List.nth result 2) "x" "int" 10));
   "test find_defs_in_fun_all_glob" >:: (fun _ -> let result = find_defs_in_fun_all_glob "main" (Frontc.parse "test.c" ())
                                                  in assert_equal (List.length result) 2;
                                                     assert_bool "check result of first" (check_result (List.hd result) "x" "int" 10);
                                                     assert_bool "check result of second" (check_result (List.nth result 1) "y" "char" 8));
   "test find_defs_all" >:: (fun _ -> let result = find_defs_all (Frontc.parse "test3.c" ())
                                      in assert_equal (List.length result) 3;
                                         assert_bool "check result of first" (check_result (List.hd result) "new_var" "struct var" 5);
                                         assert_bool "check result of second" (check_result (List.nth result 1) "new_var" "struct var" 8);
                                         assert_bool "check result of third" (check_result (List.nth result 2) "copy_var" "struct var" 13));
   "test find vars in variable-length array-declaration" >:: (fun _ -> let result = find_uses "i" (-1) (Frontc.parse "test15.c" ()) false
                                                                       in assert_equal (List.length result) 2;
                                                                          assert_bool "check result of first" (check_result (List.hd result) "i" "int" 6);
                                                                          assert_bool "check result of second" (check_result (List.nth result 1) "i" "int" 7));
   "test find defs" >:: (fun _ -> let result = find_defs "global" (-1) (Frontc.parse "test2.c" ())
                                  in assert_equal (List.length result) 3;
                                     assert_bool "check result of first" (check_result (List.hd result) "global" "char" 1);
                                     assert_bool "check result of second" (check_result (List.nth result 1) "global" "char" 4);
                                     assert_bool "check result of third" (check_result (List.nth result 2) "global" "char" 17));
   "test correct differentiation of user-defined and cil-generated variables 1" >:: 
   (fun _ -> let result = find_uses "i" (-1) (Frontc.parse "test17.c" ()) false
            in assert_equal (List.length result) 6; (* i++ is causing duplicates *)
               assert_bool "check result of first" (check_result (List.hd result) "i" "double" 9);
               assert_bool "check result of second" (check_result (List.nth result 1) "i" "double" 10);
               assert_bool "check result of third" (check_result (List.nth result 2) "i___0" "int" 4);
               assert_bool "check result of fourth" (check_result (List.nth result 3) "i___0" "int" 4));
   "test correct differentiation of user-defined and cil-generated variables 2" >::
   (fun _ -> let result = find_uses "i___0" (-1) (Frontc.parse "test17.c" ()) false
            in assert_equal (List.length result) 2;
               assert_bool "check result of first" (check_result (List.hd result) "i___1" "int" 7);
               assert_bool "check result of second" (check_result (List.nth result 1) "i___1" "int" 11))
]