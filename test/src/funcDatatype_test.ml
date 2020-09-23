open OUnit2
open Cil
open SyntacticalAnalyzer.FuncDatatype

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

let file1 = {fileName = "file";
             globals = globals1;
             globinit = None;
             globinitcalled = false}

let location2 = {line = 17;
                 file = "mapping.c";
                 byte = 98}

let enuminfo1 = {ename = "seasons";
                 eitems = [];
                 eattr = [];
                 ereferenced = true;
                 ekind = IInt}

let genumtag1 = GEnumTag(enuminfo1, location2)

let fundec1 = {svar = (makeVarinfo false "factorial" (TInt(IInt,[])));
               sformals = [];
               slocals = [];
               smaxid = 65;
               sbody = ({battrs = []; bstmts = []});
               smaxstmtid = None;
               sallstmts = []}

let gfun1 = GFun(fundec1, location2)

let globals2 = [gfun1; genumtag1; gtyp1]

let file2 = {fileName = "anotherFile";
             globals = globals2;
             globinit = None;
             globinitcalled = false}

let varinfo1 = {vname = "yeet"; 
                  vtype = TInt(IInt,[]); 
                  vattr = [];
                  vstorage = NoStorage;
                  vglob = true;
                  vinline = false;
                  vdecl = location1;
                  vinit = {init = None};
                  vid = 3;
                  vaddrof = false;
                  vreferenced = true;
                  vdescr = Pretty.nil;
                  vdescrpure = true;
                  vhasdeclinstruction = true}

let varinfo2 = {vname = "useful"; 
                  vtype = TInt(IInt,[]); 
                  vattr = [];
                  vstorage = NoStorage;
                  vglob = true;
                  vinline = false;
                  vdecl = location1;
                  vinit = {init = None};
                  vid = 4;
                  vaddrof = false;
                  vreferenced = true;
                  vdescr = Pretty.nil;
                  vdescrpure = true;
                  vhasdeclinstruction = true}

let gvar1 = GVar(varinfo1, {init = None}, location1)
let gvar2 = GVar(varinfo2, {init = None}, location2)
let gvar3 = GVar( (makeVarinfo false "notUseful" (TFloat(FComplexLongDouble, []))), {init = None}, location2)

let globals3 = [gfun1; gvar1; gvar3; GText("//TODO"); gvar2]

let print_varids gvar = 
match gvar with GVar(info,_,_) -> Printf.printf "id: %i\n" info.vid
            | _ -> ()

let check_result res res_name res_typ res_line =
match res with (name, loc, typ, id) -> if (String.compare name res_name = 0)
                                      &&(String.compare typ res_typ = 0)
                                      &&(loc.line == res_line) then true else false

let funcdatatype_tests = "test suite for func_Datatype" >::: [
    "test find_in_globals" >:: (fun _ -> assert_equal (find_in_globals globals3 "int") [3;4]);
    "test find_fundec" >:: (fun _ -> assert_equal (find_fundec globals3 "factorial") (Some(fundec1)));
    "test find_uses_in_noncond" >:: (fun _ -> let result = find_uses_in_noncond "other_int" (Frontc.parse "test6.c" ())
                                              in assert_equal (List.length result) 4;
                                                 assert_bool "check result of first" (check_result (List.hd result) "global" "other_int" 3);
                                                 assert_bool "check result of second" (check_result (List.nth result 1) "global" "other_int" 7);
                                                 assert_bool "check result of third" (check_result (List.nth result 2) "loc" "other_int" 13);
                                                 assert_bool "check result of fourth" (check_result (List.nth result 3) "loc" "other_int" 16))
]