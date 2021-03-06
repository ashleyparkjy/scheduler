(**
   This test file tests the [userSurvey.ml], [courseJson.ml], [command.ml],
   and [classes.ml] module. This test file utilizes black box testing to ensure
   the correctness of the public functions (the ones specified in the .mli
   files), via rigorous testing of common cases as well as edge cases.

    Our main programming methodology was to utilize test-driven development-
    writing test case first, and then filling in code to make the test cases
    pass. We attempted to write tests that "touched" every single possible
    outcome of our public functions. Furthermore, our tests were regularly
    updated during debugging. Every time a bug was caught, a test case was
    written to catch it for future references.

    Due to our rigorous testing methodolgy, our tests show to a high level of
    confidence that the individual units of our program are operating
    correctly. As a result our unit tests in conjunction with intense
    "demo-ing" of our system as a whole demonstrates the correctness of our
    system.

    See [schedule_test.ml] and [algorithm_test.ml] for additional tests. Note
    that two modules, [main.ml] and [visualize.ml] were not tested. This is
    because these modules mainly produce units as their outputs, because they
    rely heavily on visually outputting elements to the terminal window. For
    this reason,  Main and Visualize were tested manually via rigorous
    "demo-ing", using [make launch].
*)

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

let state_class1 = take_class init_state ["CS";"3110"]
let state_class2 = take_class state_class1 ["MATH";"2930"] 
let state_class3 = delete_class state_class2 ["CS";"3110"]
let state_sem1 = take_sem init_state ["SP20"]
let state_sem2 = take_sem state_sem1 ["FA19"]
let state_sem3 = delete_sem state_sem1 ["SP20"]
let take_sem1 = take_sem init_state ["SP20"]
let take_class1 = take_class take_sem1 ["CS";"3110"]
let take_class2 = take_class take_class1 ["MATH";"2930"] 
let take_class_time1 = take_class_time take_class2 ["11:00";"18:00"]
let userSurvey_tests = 
  [
    "get_classes & take_class test 1" >:: (fun _ -> assert_equal [("CS","3110")] (get_classes state_class1));
    "get_classes & take_class test 2" >:: (fun _ -> assert_equal [("CS","3110");("MATH","2930")] (get_classes state_class2));
    "get_classes & delete_class test 3" >:: (fun _ -> assert_equal [("MATH","2930")] (get_classes state_class3));
    "is_valid_sem test 1: wrong number 1" >:: (fun _ -> assert_equal false (["SP234"]|>is_valid_sem));
    "is_valid_sem test 1: wrong number 2" >:: (fun _ -> assert_equal false (["SPSP"]|>is_valid_sem));
    "is_valid_sem test 2: wrong front letters 1" >:: (fun _ -> assert_equal false (["SA20"]|>is_valid_sem));
    "is_valid_sem test 2: wrong front letters 2" >:: (fun _ -> assert_equal false (["2020"]|>is_valid_sem));
    "is_valid_sem test 3: wrong front letters & wrong number" >:: (fun _ -> assert_equal false (["SAS202"]|>is_valid_sem));
    "is_valid_sem test 4: correct front letters & number 1" >:: (fun _ -> assert_equal true (["SP20"]|>is_valid_sem));
    "is_valid_sem test 5: correct front letters & number 2" >:: (fun _ -> assert_equal true (["FA21"]|>is_valid_sem));
    "is_valid_class test 1: wrong number 1" >:: (fun _ -> assert_equal false (["CS"; "123"]|>is_valid_class));
    "is_valid_class test 2: wrong number 2" >:: (fun _ -> assert_equal false (["CS"; "12345"]|>is_valid_class));
    "is_valid_class test 4: wrong number 3" >:: (fun _ -> assert_equal false (["CS"; "ABCD"]|>is_valid_class));
    "is_valid_class test 3: wrong name 1" >:: (fun _ -> assert_equal false (["C"; "1234"]|>is_valid_class));
    "is_valid_class test 4: wrong name 2" >:: (fun _ -> assert_equal false (["ABCDEF"; "1234"]|>is_valid_class));
    "is_valid_class test 4: wrong name 3" >:: (fun _ -> assert_equal false (["A1"; "1234"]|>is_valid_class));
    "get_class_time 1 " >:: (fun _ -> assert_equal (660, 1080) (take_class_time1|>get_class_time));
    "is_valid_class time test 1: correct" >:: (fun _ -> assert_equal true (["09:00"; "18:00"]|>is_valid_class_time));
    "is_valid_class time test 2: start time > end time" >:: (fun _ -> assert_equal false (["18:00"; "09:00"]|>is_valid_class_time));
    "is_valid_class time test 3: non-number" >:: (fun _ -> assert_equal false (["ab:cd"; "09:00"]|>is_valid_class_time));
    "is_valid_class time test 4: wrong format" >:: (fun _ -> assert_equal false (["0900"; "1800"]|>is_valid_class_time));
    "is_valid_class time test 5: wrong range" >:: (fun _ -> assert_equal false (["09:00"; "25:00"]|>is_valid_class_time));
    "is_valid_class time test 5: wrong range" >:: (fun _ -> assert_equal false (["09:00"; "22:61"]|>is_valid_class_time));
  ]


let courseJson_tests = [
  "make_url" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        (CourseJson.make_url "SP20" "CS" "3110")
        "https://classes.cornell.edu/api/2.0/search/classes.json?roster=SP20&subject=CS&q=3110");
  "make_url empty" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        (CourseJson.make_url "" "" "")
        "https://classes.cornell.edu/api/2.0/search/classes.json?roster=&subject=&q=");
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
    userSurvey_tests;
    courseJson_tests;
    classes_tests;
  ]

let _ = run_test_tt_main suite
