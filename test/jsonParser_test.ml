open OUnit2
open JsonParser

let query = {sel = [Name_sel; ID_sel;Location_sel];
              k = Datatype_k;
              tar = Name_t("int");
              f = Uses_f;
              str =Fun_s("mergeSort");
              lim = None_c}

let query1 = {sel = [Type_sel; ID_sel; Name_sel];
              k = Fun_k;
              tar = ID_t(4447);
              f = UsesWithVar_f("tmp");
              str = None_s;
              lim = Constraint_c("tmp == 'E'")}

let tests = "test suite for JSONParser" >::: [
  "typical query"  >:: (fun _ -> assert_equal query (parse_json_file "test/test.json"));
  "random lowcase and uppercase" >:: (fun _ -> assert_equal query1 (parse_json_file "test/test1.json"))
]

let _ = run_test_tt_main tests