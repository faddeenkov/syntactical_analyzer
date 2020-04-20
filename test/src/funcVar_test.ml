open OUnit2
open Mylib.FuncVar
open Cil

let location1 = {line = 1;file = "test";byte = 0}

let lhost1 = Var({vname = "x"; 
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
                  vhasdeclinstruction = true})

let result1 = ("x", location1, "int", 0)::[]

let expression1 = Lval(lhost1, NoOffset)

let funcvar_tests = "test suite for sum" >::: [
  "search lhost1"  >:: (fun _ -> assert_equal (search_lhost lhost1 "x" location1) result1);
  "search expression1" >:: (fun _ -> assert_equal (search_expression expression1 "x" location1) result1)
]