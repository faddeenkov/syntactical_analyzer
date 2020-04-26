open OUnit2
open Testlib.JsonParser_test
open Testlib.FuncVar_test
open Testlib.FuncDatatype_test

let _ = run_test_tt_main parser_tests; 
        run_test_tt_main funcvar_tests;
        run_test_tt_main funcdatatype_tests