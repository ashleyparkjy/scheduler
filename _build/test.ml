open OUnit2
open UserSurvey
open Command
open CourseJson
open Classes

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') ->
        if n=100 then acc ^ "..."  (* stop printing long list *)
        else loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let courseJson_tests = [
  "make_url" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        (CourseJson.make_url "SP20" "CS" "3110")
        "https://classes.cornell.edu/api/2.0/search/classes.json?roster=SP20&subject=CS&q=3110");
  "make_url empty" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        (CourseJson.make_url "" "" "")
        "https://classes.cornell.edu/api/2.0/search/classes.json?roster=&subject=&q=");

(*
  "get_json" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        ("https://classes.cornell.edu/api/2.0/config/acadCareers.json?roster=SP20"
         |> CourseJson.get_json |> Lwt_main.run)
        "{\"status\":\"success\",\"data\":{\"acadCareers\":[{\"value\":\"UG\",\"descr\":\"Undergraduate\"},{\"value\":\"GR\",\"descr\":\"Graduate\"},{\"value\":\"GM\",\"descr\":\"Graduate Management\"},{\"value\":\"LA\",\"descr\":\"Law\"},{\"value\":\"VM\",\"descr\":\"Veterinary Medicine\"}]},\"message\":null,\"meta\":{\"copyright\":\"Cornell University, Office of the University Registrar\",\"referenceDttm\":\"2020-04-16T19:14:21-0400\"}}");
*)

  "runner produces exn if query is bad" >:: (fun _ -> 
      assert_raises (BadUrl 404)
        (fun()-> (
             CourseJson.runner "SP20" "CS" "31jj10"
           )));
]

let classes_tests = 
  let j = Yojson.Basic.from_file "testCS.json" and
  k = Yojson.Basic.from_file "testMATH.json" and
  l = Yojson.Basic.from_file "testPT.json" and
  m = Yojson.Basic.from_file "testPT_units.json" in [
    "bad query" >:: (fun _ -> 
        assert_raises (BadQuery)
          (fun()-> (
               empty |> from_json (Yojson.Basic.from_file "testEMPTY.json")
             )));

    "check empty" >:: (fun _ -> 
        assert_equal
          (empty |> is_empty) true);

    "add class to roster" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> is_empty) false);

    "size 0" >:: (fun _ -> 
        assert_equal
          (empty |> size) 0);

    "size 1" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> size) 1);
    "size 2" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> size) 2);

    "course id none" >:: (fun _ -> 
        assert_equal
          (empty |> course_ids) []);
    "course id singular" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> course_ids) [358556]);
    "course ids" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists
          (empty |> from_json j |> from_json k |> course_ids) [358556;352295]);

    "cs subject id" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> subject 358556) "CS");
    "math subject id" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> subject 352295) "MATH");

    "cs catalog" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> catalog_number 358556) 3110);
    "math catalog" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> catalog_number 352295) 2930);

    "cs short title" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> title_short 358556) "Data Struct & Functional Progr");
    "math short title" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> title_short 352295) "Differential Equations Engrs");

    "cs long title" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> title_long 358556) "Data Structures and Functional Programming");
    "math long title" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> title_long 352295) "Differential Equations for Engineers");

    "cs sections" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists
          (empty |> from_json j |> from_json k |> sections 358556) [10601;10602;10603;10604;11338;11549;11551;11552;12387;12388;12389;12401;12703;19117;19118]);
    "func on bad class" >:: (fun _ -> 
        assert_raises (ClassNotFound 000000)
          (fun()-> (
               empty |> from_json j |> from_json k |> sections 000000
             )));

    "cs lecture sec" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> section_type 10601 358556) "LEC");
    "cs discussion sec" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> section_type 10602 358556) "DIS");

    "cs lecture section number" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> section_number 10601 358556) "001");
    "cs dis sec num" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> section_number 10602 358556) "201");

    "cs lecture meetings" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists
          (empty |> from_json j |> from_json k |> meetings 10601 358556) [1]);

    "func on bad section" >:: (fun _ -> 
        assert_raises (SectionNotFound 000000)
          (fun()-> (
               empty |> from_json j |> from_json k |> meetings 000000 358556
             )));

    "cs lecture start" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> start_time 1 10601 358556) {min=10;hr=10;});
    "cs lecture end" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> end_time 1 10601 358556) {min=0;hr=11;});
    "cs lecture start2" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> start_time 1 10603 358556) {min=30;hr=14;});
    "cs lecture end2" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> end_time 1 10603 358556) {min=20;hr=15;});
    "cs lecture start3" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> start_time 1 12388 358556) {min=20;hr=12;});

    "cs instructor" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> instructors 1 12388 358556) ["jnf27"]);

    "func on bad meeting" >:: (fun _ -> 
        assert_raises (MeetingNotFound 0)
          (fun()-> (
               empty |> from_json j |> from_json k |> instructors 0 12388 358556
             )));

    "cs instructor name" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> instructor_name "jnf27" 1 12388 358556) "Nate Foster");

    "func on bad instructor" >:: (fun _ -> 
        assert_raises (InstructorNotFound "blah")
          (fun()-> (
               empty |> from_json j |> from_json k |> instructor_name "blah" 1 12388 358556
             )));

    "cs lecture pattern" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists
          (empty |> from_json j |> from_json k |> pattern 1 10601 358556) [Tuesday;Thursday]);
    "cs dis pattern" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists
          (empty |> from_json j |> from_json k |> pattern 1 10602 358556) [Monday;Wednesday]);

    "cs lecture facility" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> facility_description 1 10601 358556) "Kennedy Hall 116-Call Aud");
    "cs dis facility" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> facility_description 1 10602 358556) "Hollister Hall 320");

    "cs lecture bldg" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> building_description 1 10601 358556) "Kennedy Hall");
    "cs dis bldg" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> building_description 1 10602 358556) "Hollister Hall");

    "cs campus" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> campus 10601 358556) "MAIN");

    "cs units" >:: (fun _ -> 
        assert_equal
          (empty |> from_json j |> from_json k |> units 358556) 4);
    "cs badunits" >:: (fun _ -> 
        assert_raises (BadUnits 358236)
          (fun()-> (
               empty |> from_json m |> units 358236
             )));
    "cs badtime" >:: (fun _ -> 
        assert_raises (BadTime "")
          (fun()-> (
               empty |> from_json l
             )));

    "cs components" >:: (fun _ -> 
        assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
          (empty |> from_json j |> from_json k |> components_required 358556) ["LEC";"DIS"]);

    "cs description" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> from_json j |> from_json k |> description 358556)
          "Advanced programming course that emphasizes functional programming techniques and data structures. Programming topics include recursive and higher-order procedures, models of programming language evaluation and compilation, type systems, and polymorphism. Data structures and algorithms covered include graph algorithms, balanced trees, memory heaps, and garbage collection. Also covers techniques for analyzing program performance and correctness."
      );
  ]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [
    courseJson_tests;
    classes_tests;
  ]

let _ = run_test_tt_main suite
