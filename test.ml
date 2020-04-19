open OUnit2
open UserSurvey
open Command

let userSurveytests = 
  [
    (* take_class testing *)
    (* class_is_valid testing *)
    (* delete_class testing *)
  ]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [
    userSurveytests;
  ]

let _ = run_test_tt_main suite
