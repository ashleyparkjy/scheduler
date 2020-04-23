open OUnit2
open Schedule

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



let schedule_tests = 
  let j = Yojson.Basic.from_file "testCS.json" and
  k = Yojson.Basic.from_file "testMATH.json" in
  let x = Classes.empty |> Classes.from_json j |> Classes.from_json k in
  [
    "empty true" >:: (fun _ -> 
        assert_equal (empty |> is_empty) true);
    "empty size 0" >:: (fun _ -> 
        assert_equal (empty |> size) 0);
    "1 element non-empty" >:: (fun _ -> 
        assert_equal (empty |> add_section 10601 358556 x |> is_empty) false);
    "1 element size" >:: (fun _ -> 
        assert_equal (empty |> add_section 10601 358556 x |> size) 1);
    "1 element name" >:: (fun _ -> 
        assert_equal ~printer:(pp_string)
          (empty |> add_section 10601 358556 x |> peak).course_name "Data Structures and Functional Programming");
    "1 element instructors" >:: (fun _ -> 
        assert_equal ~printer:(pp_list pp_string)
          (empty |> add_section 10601 358556 x |> peak).instructors ["Nate Foster"]);
  ]

let suite =
  "test suite for Cornell Scheduler (schedule module)"  >::: List.flatten [
    schedule_tests;
  ]

let _ = run_test_tt_main suite
