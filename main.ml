
(** [survey_semester] is a string that represents the semester of the
    classes. *)
let survey_semester =
  "SP20"

(** [survey_classes] is a tuple of surveyed classes. *)
let survey_classes =
  ["CS","3110";"MATH","2930";"CS","2800";]

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

let main () = 
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Cornell Scheduler.\n");
  (*UserSurvey.init_state |> UserSurvey.prompt_semester;*)

  let s = survey_semester and
    c = survey_classes in
  let r = get_class s c Classes.empty in
  List.map (fun x-> print_endline (string_of_int x)) (Classes.course_ids r) |> ignore;
  ()

let () = main ()
