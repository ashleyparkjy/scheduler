
let survey_semester =
  "SP20"

let survey_classes =
  ["CS","3110";"MATH","2930";"CS","2800"]

let rec get_class s c =
  match c with
  | [] -> ()
  | (a,b)::t -> CourseJson.runner s a b; get_class s t

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Cornell Scheduler.\n");

  let s = survey_semester and
    c = survey_classes in
  get_class s c

let () = main ()
