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

let fundec1 = {svar = (makeVarinfo false "yeet" (TInt(IInt,[])));
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

let funcdatatype_tests = "test suite for func_Datatype" >::: [
    "test find_userdef_iter_list"  >:: (fun _ -> assert_equal (find_userdef_iter_list "BYTE" globals1) (Some(location1, "BYTE")));
    "test find_decl" >:: (fun _ -> assert_equal (find_decl "someStruct" file1) ("", location1, "someStruct", -1));
    "test find_decl_all_glob 1" >:: (fun _ -> assert_equal (find_decl_all_glob file1) [("",location1,"BYTE",-1);("",location1,"someStruct",-1)]);
    "test find_decl_all_glob 2" >:: (fun _ -> assert_equal (find_decl_all_glob file2) [("", location2, "seasons",-1);("", location1, "BYTE",-1)]); 
    "test find_in_globals" >:: (fun _ -> assert_equal (find_in_globals globals3 "int") [3;4])
]