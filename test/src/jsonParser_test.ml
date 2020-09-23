open OUnit2
open SyntacticalAnalyzer.JsonParser

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

let query2 = {sel = [Name_sel; Location_sel; Type_sel; ID_sel];
              k = Var_k;
              tar = And_t(["a"; "b"]);
              f = Uses_f;
              str = Fun_s("ggt");
              lim = Constraint_c("a/b == 2")}

let query3 = {sel = [Location_sel];
              k = Fun_k;
              tar = All_t;
              f = Returns_f;
              str = None_s;
              lim = None_c}


let parser_tests = "test suite for JSONParser" >::: [
  "typical query"  >:: (fun _ -> assert_equal query (parse_json_file "test.json"));
  "random lowcase and uppercase" >:: (fun _ -> assert_equal query1 (parse_json_file "test1.json"));
  "query with and" >:: (fun _ -> assert_equal query2 (parse_json_file "test2.json"));
  "function-returns query" >:: (fun _ -> assert_equal query3 (parse_json_file "test3.json"));
  "bad query" >:: (fun _ -> assert_raises (Error("JsonParser.structure")) (fun _ -> (parse_json_file "test4.json")))
]
