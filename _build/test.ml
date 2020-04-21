open OUnit2
open UserSurvey
open Command

let state = take_class init_state ["CS";"3110"]
let state2 = take_class state ["MATH";"2930"] 
let state3 = take_sem state2 ["SP20"]


let userSurveytests = 
  [
    "get_classes test 1" >:: (fun _ -> assert_equal [("CS","3110");("MATH","2930")] (get_classes state3));
    "get_semester test 2" >:: (fun _ -> assert_equal "SP20" (get_semester state3));
    (* take_class testing *)
    (* class_is_valid testing *)
    (* delete_class testing *)
  ]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [
    userSurveytests;
  ]

let _ = run_test_tt_main suite
