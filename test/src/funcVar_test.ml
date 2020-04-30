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

let check_test8_first x = 
match x with (name, loc, typ, id) -> if (String.compare name "x" = 0)
                                      &&(String.compare typ "int" = 0)
                                      &&(loc.line == 7) then true else false

let check_test8_second x = 
match x with (name, loc, typ, id) -> if (String.compare name "y" = 0)
                                      &&(String.compare typ "char" = 0)
                                      &&(loc.line == 8) then true else false

let check_test9_first x = 
match x with (name, loc, typ, id) -> if (String.compare name "global" = 0)
                                      &&(String.compare typ "int" = 0)
                                      &&(loc.line == 5) then true else false

let check_test9_second x = 
match x with (name, loc, typ, id) -> if (String.compare name "formal" = 0)
                                      &&(String.compare typ "float" = 0)
                                      &&(loc.line == 5) then true else false

let check_test9_third x = 
match x with (name, loc, typ, id) -> if (String.compare name "local" = 0)
                                      &&(String.compare typ "float" = 0)
                                      &&(loc.line == 4) then true else false

let check_test9_fourth x = 
match x with (name, loc, typ, id) -> if (String.compare name "local" = 0)
                                      &&(String.compare typ "float" = 0)
                                      &&(loc.line == 5) then true else false

let check_test10_first x = 
match x with (name, loc, typ, id) -> if (String.compare name "global" = 0)
                                      &&(String.compare typ "char" = 0)
                                      &&(loc.line == 4) then true else false

let check_test10_second x = 
match x with (name, loc, typ, id) -> if (String.compare name "global" = 0)
                                      &&(String.compare typ "char" = 0)
                                      &&(loc.line == 8) then true else false

let check_test10_third x = 
match x with (name, loc, typ, id) -> if (String.compare name "global" = 0)
                                      &&(String.compare typ "char" = 0)
                                      &&(loc.line == 17) then true else false

let check_test11_first_and_second x = 
match x with (name, loc, typ, id) -> if (String.compare name "new_var" = 0)
                                      &&(String.compare typ "struct var" = 0)
                                      &&(loc.line == 8) then true else false

let check_test11_third x = 
match x with (name, loc, typ, id) -> if (String.compare name "new_var" = 0)
                                      &&(String.compare typ "struct var" = 0)
                                      &&(loc.line == 13) then true else false

let check_test11_fourth x = 
match x with (name, loc, typ, id) -> if (String.compare name "copy_var" = 0)
                                      &&(String.compare typ "struct var" = 0)
                                      &&(loc.line == 13) then true else false
                                      
let check_test12_first x = 
match x with (name, loc, typ, id) -> if (String.compare name "i" = 0)
                                      &&(String.compare typ "int" = 0)
                                      &&(loc.line == 4) then true else false

let check_test12_second x = 
match x with (name, loc, typ, id) -> if (String.compare name "i" = 0)
                                      &&(String.compare typ "int" = 0)
                                      &&(loc.line == 5) then true else false

let check_test12_third x = 
match x with (name, loc, typ, id) -> if (String.compare name "i" = 0)
                                      &&(String.compare typ "int" = 0)
                                      &&(loc.line == 10) then true else false

let funcvar_tests = "test suite for func_Var" >::: [
  "search lhost1"  >:: (fun _ -> assert_equal (search_lhost lhost1 "x" location1 (-1)) result1);
  "search lhost1 by id" >:: (fun _ -> assert_equal (search_lhost lhost1 "" location1 0) result1);
  "search expression1" >:: (fun _ -> assert_equal (search_expression expression1 "x" location1 (-1)) result1);
  "search expression2" >:: (fun _ -> assert_equal (search_expression expression3 "y" location2 (-1)) result2);
  "search expression-list1" >:: (fun _ -> assert_equal (search_expression_list expr_list "x" location1 (-1)) result4);
  "search instruction-list1" >:: (fun _ -> assert_equal (search_instr_list_for_var instr_list "x" (-1)) result4);
  "search statement-list1" >:: (fun _ -> assert_equal (search_stmt_list_for_var stmt_list "x" (-1)) result5);
  "test find_uses_in_fun_all_glob" >:: (fun _ -> let result = find_uses_in_fun_all_glob "main" (Frontc.parse "test.c" ())
                                                  in assert_equal (List.length result) 2;
                                                     assert_bool "check result of first" (check_test8_first (List.hd result));
                                                     assert_bool "check result of second" (check_test8_second (List.nth result 1)));
  "test find_uses_in_fun_all" >:: (fun _ -> let result = find_uses_in_fun_all "niceFunction" (Frontc.parse "test1.c" ())
                                            in assert_equal (List.length result) 4;
                                               assert_bool "check result of first" (check_test9_first (List.hd result));
                                               assert_bool "check result of second" (check_test9_second (List.nth result 1));
                                               assert_bool "check result of third" (check_test9_third (List.nth result 2));
                                               assert_bool "check result of fourth" (check_test9_fourth (List.nth result 3)));
  "test find_uses" >:: (fun _ -> let result = find_uses "global" (-1) (Frontc.parse "test2.c" ())
                                 in assert_equal (List.length result) 3;
                                    assert_bool "check result of first" (check_test10_first (List.hd result));
                                    assert_bool "check result of second" (check_test10_second (List.nth result 1));
                                    assert_bool "check result of third" (check_test10_third (List.nth result 2)));
   "test find_uses_all" >:: (fun _ -> let result = find_uses_all (Frontc.parse "test3.c" ())
                                      in assert_equal (List.length result) 4;
                                         assert_bool "check result of first" (check_test11_first_and_second (List.hd result));
                                         assert_bool "check result of second" (check_test11_first_and_second (List.nth result 1));
                                         assert_bool "check result of third" (check_test11_third (List.nth result 2));
                                         assert_bool "check result of fourth" (check_test11_fourth (List.nth result 3)));    
   "test find_uses_in_cond" >:: (fun _ -> let result = find_uses_in_cond "i" (-1) (Frontc.parse "test4.c" ())
                                          in assert_equal (List.length result) 3;
                                             assert_bool "check result of first" (check_test12_first (List.hd result));
                                             assert_bool "check result of second" (check_test12_second (List.nth result 1));
                                             assert_bool "check result of third" (check_test12_third (List.nth result 2)))                                            
]