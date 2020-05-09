(**
   This test file tests the [algorithm.ml] module. This test file utilizes black
   box testing to ensure the correctness of the public functions (the ones
   specified in the .mli files), via rigorous testing of common cases as well
   as edge cases.

    See [test.ml] for information on our system testing as a whole.
*)

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
let l = Yojson.Basic.from_file "testAEM.json"
let m = Yojson.Basic.from_file "test2940.json"
let n = Yojson.Basic.from_file "testKOREA.json"
let x = Classes.empty |> Classes.from_json j |> Classes.from_json k |> Classes.from_json l |> Classes.from_json m

(** [koreacs_sched] is unqiue schedule list of korea 1110 and cs 3110 *)
let koreacs_sched = 
  let jsons = Classes.empty |> Classes.from_json j |> Classes.from_json n in
  let raw = jsons |> Schedule.schedule_maker in
  let refined_schedule = raw |> Algorithm.filter_valid_schedule [] in
  refined_schedule |> Algorithm.delete_dups []

(* Schedule 1
   - MATH 2930 Lecture: MWF 12:20 - 1:10PM
   - MATH 2930 Discussion: Tr 1:25 - 2:15PM
   - CS 3110 Lecture: TT 10:10 - 11:00AM
   - CS 3110 Discussion: TT 12:20 - 1:10PM *)
let schedule1 = empty |> add_section 10601 358556 x |> add_section 12401 358556 x
                |> add_section 5326 352295 x |> add_section 5332 352295 x

(* Schedule 2 - Conflict
   - MATH 2930 Lecture: MWF 12:20 - 1:10PM
   - MATH 2930 Discussion: Tr 12:20 - 1:10PM
   - CS 3110 Lecture: TT 10:10 - 11:00AM
   - CS 3110 Discussion: 12:20 - 1:10PM *)
let schedule2 = empty |> add_section 10601 358556 x |> add_section 12401 358556 x
                |> add_section 5326 352295 x |> add_section 5330 352295 x 

(* Schedule 3 - Lunch Time 1day
   - MATH 2930 Lecture: MWF 11:15 - 12:05PM
   - MATH 2930 Discussion: Tr 1:25 - 2:15PM
   - CS 3110 Lecture: TT 10:10 - 11:00AM
   - CS 3110 Discussion: TT 12:20 - 1:10PM 
   - MATH 2940 Lecture: MWF 12:20 - 1:10PM
   - MATH 2940 Discussion: Tr 11:15 - 12:05PM*)
let schedule3 = empty |> add_section 10601 358556 x |> add_section 12401 358556 x
                |> add_section 5325 352295 x |> add_section 5332 352295 x
                |> add_section 5365 352307 x |> add_section 5371 352307 x

(* Schedule 4 - Lunch Time 1day
   - MATH 2930 Lecture: MWF 11:15 - 12:05PM
   - MATH 2930 Discussion: Tr 1:25 - 2:15PM
   - CS 3110 Lecture: TT 10:10 - 11:00AM
   - CS 3110 Discussion: TT 12:20 - 1:10PM 
   - MATH 2940 Lecture: MWF 12:20 - 1:10PM
   - MATH 2940 Discussion: Tr 11:15 - 12:05PM
   - AEM 3251 Lecutre: MW 10:10 - 11:00PM 
   - AEM 3251 Discussion: MW 1:25 - 2:15PM *)
let schedule4 = empty |> add_section 10601 358556 x |> add_section 12401 358556 x
                |> add_section 5325 352295 x |> add_section 5332 352295 x
                |> add_section 5365 352307 x |> add_section 5371 352307 x        
                |> add_section 15966 368416 x |> add_section 15968 368416 x

let schedulelst = [schedule1; schedule2]
let schedulelst2 = [schedule1; schedule3; schedule4]
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
  "score_lunch test 1" >:: (fun _ -> assert_equal 1. (schedule1 |> score_lunch));
  "score_lunch test 2" >:: (fun _ -> assert_equal 0.2 (schedule3 |> score_lunch));
  "day_class_time test 1" >:: (fun _ -> assert_equal 100 (schedule3 |> Schedule.get_monday |> day_class_time 0));
  "day_class_time test 2" >:: (fun _ -> assert_equal 100 (schedule3 |> Schedule.get_tuesday |> day_class_time 0));
  "day_class_time test 3" >:: (fun _ -> assert_equal 100 (schedule3 |> Schedule.get_wednesday |> day_class_time 0));
  "day_class_time test 4" >:: (fun _ -> assert_equal 200 (schedule3 |> Schedule.get_thursday |> day_class_time 0));
  "day_class_time test 5" >:: (fun _ -> assert_equal 100 (schedule3 |> Schedule.get_friday |> day_class_time 0));
  "score_spread test 1" >:: (fun _ -> assert_equal (sqrt(1600.)/.sqrt(46080.)) (schedule3 |> score_spread));
  "score_spread test 2" >:: (fun _ -> assert_equal (sqrt(2400.)/.sqrt(81920.)) (schedule4 |> score_spread));
  "score_class_time test 1" >:: (fun _ -> assert_equal ~cmp: cmp_float 0.5 (schedule3 |> score_classtime (600,780)));
  "score_class_time test 2" >:: (fun _ -> assert_equal ~cmp: cmp_float 0.9 (schedule3 |> score_classtime (600,800)));
  "score_class_time test 3" >:: (fun _ -> assert_equal ~cmp: cmp_float 0.7 (schedule3 |> score_classtime (660,800)));
  "delete dups" >:: (fun _ -> assert_equal (koreacs_sched |> List.length) 8);
]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [
    algorithm_tests;
  ]

let _ = run_test_tt_main suite