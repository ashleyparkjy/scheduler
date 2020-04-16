open UserSurvey

let main () = 
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Cornell Scheduler.\n");
  prompt_class init_state

let () = main ()
