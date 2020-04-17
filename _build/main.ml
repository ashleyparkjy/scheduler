
let get_class x =
  if x = true then CourseJson.runner "SP20" "CS" "31jj10"
  else ()


let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Cornell Scheduler.\n");
  get_class true

let () = main ()
