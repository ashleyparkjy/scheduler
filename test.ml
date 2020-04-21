open OUnit2
open UserSurvey
open Command

let state_class1 = take_class init_state ["CS";"3110"]
let state_class2 = take_class state_class1 ["MATH";"2930"] 
let state_class3 = delete_class state_class2 ["CS";"3110"]
let state_sem1 = take_sem init_state ["SP20"]
let state_sem2 = take_sem state_sem1 ["FA19"]
let state_sem3 = delete_sem state_sem1 ["SP20"]


let userSurveytests = 
  [
    "init_state test 1" >:: (fun _ -> assert_equal "" (get_semester init_state));
    "init_state test 2" >:: (fun _ -> assert_equal [] (get_classes init_state));
    "get_semester & take_sem test 1" >:: (fun _ -> assert_equal "SP20" (get_semester state_sem1));
    "get_semester & take_sem test 2" >:: (fun _ -> assert_equal "SP20" (get_semester state_sem2));
    "get_semester & delete_sem test 3" >:: (fun _ -> assert_equal "" (get_semester state_sem3));
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


  ]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [

  ]

let _ = run_test_tt_main suite
