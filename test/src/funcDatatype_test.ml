open OUnit2
open Cil
open Mylib.FuncDatatype

let typeinfo1 = {tname = "BYTE";
                 ttype = TInt(IInt,[]);
                 treferenced = false}
let location1 = {line = 3;
                 file = "test.c";
                 byte = 10}
let gtyp1 = GType(typeinfo1, location1)

let compinfo1 = {cstruct = true;
                 cname = "someStruct";
                 ckey = 3;
                 cfields = [];
                 cattr = [];
                 cdefined = true;
                 creferenced = false}
let gcomptag1 = GCompTag(compinfo1,location1)

let globals1 = [gtyp1; GText("le bla bla"); gcomptag1]

let funcdatatype_tests = "test suite for func_Datatype" >::: [
    "test find_userdef_iter_list"  >:: (fun _ -> assert_equal (find_userdef_iter_list "BYTE" globals1) (Some(location1, "BYTE")));
]