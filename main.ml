open UserSurvey

let main () = 
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Cornell Scheduler.\n");
  prompt_semester init_state

let () = main ()
