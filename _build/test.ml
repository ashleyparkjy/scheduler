open OUnit2
open UserSurvey
open Command


let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [

  ]

let _ = run_test_tt_main suite
