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

let funcvar_tests = "test suite for sum" >::: [
  "search lhost1"  >:: (fun _ -> assert_equal (search_lhost lhost1 "x" location1) result1);
  "search expression1" >:: (fun _ -> assert_equal (search_expression expression1 "x" location1) result1);
  "search expression2" >:: (fun _ -> assert_equal (search_expression expression3 "y" location2) result2);
  "search expression-list1" >:: (fun _ -> assert_equal (search_expression_list expr_list "x" location1) result4);
  "search instruction-list1" >:: (fun _ -> assert_equal (search_instr_list_for_var instr_list "x") result4);
  "search statement-list1" >:: (fun _ -> assert_equal (search_stmt_list_for_var stmt_list "x") result5);
]