open UserSurvey

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Cornell Scheduler.\n");
  class_prompt init_state

let () = main ()
