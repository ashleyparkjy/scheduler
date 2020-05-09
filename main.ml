(** Returns a roster of type [Classes] containing data from [roster] and new
    data from the class [c] in semester [s]. *)
let rec get_class s c roster =
  match c with
  | [] -> roster
  | (a,b)::t ->
    begin
      CourseJson.runner s a b;
      let j = Yojson.Basic.from_file (s ^ "_" ^ a ^ "_" ^ b ^ ".json") in
      get_class s t (Classes.from_json j roster)
    end

(** [schedule_num] returns the number of schedules to print. *)
let rec schedule_num () = 
  ANSITerminal.(
    print_string [cyan]
      "\nInput your number of schedules to print (must be a valid integer >0 a\
       nd <number of non-conflicting schedules):\n>");
  try
    let pref = read_line () in
    let n = int_of_string pref in
    if n >= 0 then n else schedule_num ()
  with Failure _ -> schedule_num ()

let main () = 
  ANSITerminal.(print_string [cyan]
                  "\n\nWelcome to Cornell Scheduler.\n");


  let st = UserSurvey.init_state |> UserSurvey.prompt_semester
           |> UserSurvey.final_output in
  let s = st.final_semester and
    c = st.final_classes in

  let r = get_class s c Classes.empty in
  let raw_schedule = r |> Schedule.schedule_maker in
  print_endline ("Number of schedule combinations: "
                 ^ (raw_schedule |> List.length |> string_of_int));
  let refined_schedule = raw_schedule |> Algorithm.filter_valid_schedule [] in
  print_endline ("Number of non-conflicting schedule combinations: "
                 ^ (refined_schedule |> List.length |> string_of_int));
  let unique_schedule = refined_schedule |> Algorithm.delete_dups [] in
  print_endline ("Number of non-conflicting unique schedule combinations: "
                 ^ (unique_schedule |> List.length |> string_of_int));
  let n = schedule_num () in
  let top_n = Algorithm.rank_schedule n [] st unique_schedule in      
  print_endline "";
  print_endline ((top_n |> List.length |> string_of_int)
                 ^ " schedules generated.");    
  top_n |> Visualize.visualize

let () = main ()
