open OUnit2
open Schedule
open Algorithm

let (time1:Classes.time) = {
  hr=7;
  min=30;
}

let (time2:Classes.time) = {
  hr=20;
  min=30;
}

let (empty_cmp_list:comparable_event list) = []

let j = Yojson.Basic.from_file "testCS.json"
let k = Yojson.Basic.from_file "testMATH.json"
let x = Classes.empty |> Classes.from_json j |> Classes.from_json k
let schedule1 = empty |> add_section 10601 358556 x |> add_section 12401 358556 x
                |> add_section 5326 352295 x |> add_section 5332 352295 x
let schedule2 = empty |> add_section 10601 358556 x |> add_section 12401 358556 x
                |> add_section 5326 352295 x |> add_section 5330 352295 x 
let schedulelst = [schedule1; schedule2]
let thursday = (schedule1 |> get_thursday)
let thursday_cmp = thursday |> comparable_list empty_cmp_list 
let thursday_cmp_sorted = thursday_cmp |> sort_start_time
let thursday_dup = (schedule2 |> get_thursday)
let thursday_dup_cmp = thursday_dup |> comparable_list empty_cmp_list 
let thursday_dup_cmp_sorted = thursday_dup_cmp |> sort_start_time

let rec cmp_lst_starttime acc cmp_list = 
  match cmp_list with
  | hd::tl -> cmp_lst_starttime ((hd |> get_start_time) :: acc) tl 
  | _ -> acc

let algorithm_tests = [
  "time_to_min test 1" >:: (fun _ -> assert_equal 450 (time_to_min time1));
  "time_to_min test 2" >:: (fun _ -> assert_equal 1230 (time_to_min time2));
  "cmp list & sort test 1 (will output as reverse due to helper)" >:: 
  (fun _ -> assert_equal [805; 740; 610] (thursday_cmp_sorted |> cmp_lst_starttime []));
  "cmp list & sort test 2 (will output as reverse due to helper)" >:: 
  (fun _ -> assert_equal [740; 740; 610] (thursday_dup_cmp_sorted |> cmp_lst_starttime []));
  "is_dup test 1" >:: (fun _ -> assert_equal true (thursday_cmp |> is_duplicate));
  "is_dup test 2" >:: (fun _ -> assert_equal false (thursday_dup_cmp |> is_duplicate));
  "end_time_compare test 1" >:: (fun _ -> assert_equal true (thursday_cmp |> end_time_compare));
  "end_time_compare test 2" >:: (fun _ -> assert_equal false (thursday_dup_cmp |> end_time_compare));
  "check_day_schedule test 1" >:: (fun _ -> assert_equal true (thursday |> check_day_schedule));
  "check_day_schedule test 2" >:: (fun _ -> assert_equal false (thursday_dup |> check_day_schedule));
  "filter_valid_schedule test 1" >:: (fun _ -> assert_equal [schedule1] (schedulelst |> filter_valid_schedule []));

  (* test procedure take two cases where spread, lunch, or class_time would differ 
     Assuming that the student wants lunch time, class_time, and clustered together *)



  (* day_class_time *)

  (* score_spread *)

  (* score_lunch *)

  (* schedule_score *)

  (* score_class_time *)


]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [
    algorithm_tests;
  ]

let _ = run_test_tt_main suite