open OUnit2
open UserSurvey
open Command
open CourseJson

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let courseJson_tests = [
  "make_url" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        (CourseJson.make_url "SP20" "CS" "3110")
        "https://classes.cornell.edu/api/2.0/search/classes.json?roster=SP20&subject=CS&q=3110");
  "make_url empty" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        (CourseJson.make_url "" "" "")
        "https://classes.cornell.edu/api/2.0/search/classes.json?roster=&subject=&q=");

  "get_json" >:: (fun _ -> 
      assert_equal ~printer:(pp_string)
        ("https://classes.cornell.edu/api/2.0/config/acadCareers.json?roster=SP20"
         |> CourseJson.get_json |> Lwt_main.run)
        "{\"status\":\"success\",\"data\":{\"acadCareers\":[{\"value\":\"UG\",\"descr\":\"Undergraduate\"},{\"value\":\"GR\",\"descr\":\"Graduate\"},{\"value\":\"GM\",\"descr\":\"Graduate Management\"},{\"value\":\"LA\",\"descr\":\"Law\"},{\"value\":\"VM\",\"descr\":\"Veterinary Medicine\"}]},\"message\":null,\"meta\":{\"copyright\":\"Cornell University, Office of the University Registrar\",\"referenceDttm\":\"2020-04-16T19:14:21-0400\"}}");

  "runner produces exn if query is bad" >:: (fun _ -> 
      assert_raises (BadUrl 404)
        (fun()-> (
             CourseJson.runner "SP20" "CS" "31jj10"
           )));

]

let suite =
  "test suite for Cornell Scheduler"  >::: List.flatten [
    courseJson_tests;
  ]

let _ = run_test_tt_main suite
