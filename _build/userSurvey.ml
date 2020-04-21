<<<<<<< HEAD
open Command


(** The type of class_id. (ex) ["CS";"3110"]) *)
type class_id = string list

(** The type of t, user survey answers, in record *)
type t = {
  semester: string;
  classes_input: class_id list
}

(** [init_state] is the initial state of survey response where the answers to 
    all of the questions are set as empty *)
let init_state = 
  {
    semester = "";
    classes_input = []
  }

(** [get_semester st] is a string of semester for current state [st]. *)
let get_semester st = 
  st.semester

(** [get_classes st] is an association list of classes for current state [st]. *)
let get_classes st = 
  let rec to_assoc_list list acc =
    match list with
    | [] -> acc
    | h::t when List.length h = 2 -> to_assoc_list t ((List.nth h 0, List.nth h 1)::acc)
    | _ -> failwith "error" in
  to_assoc_list st.classes_input []

(** [check_length tl] is true if for the string list [tl] with two elements,
    the length of first string (ex."CS") is between 2 and 5 and the length of 
    second string (ex."3110") is 4. False otherwise. *) 
let check_length tl = 
  match tl with
  | h::t::[] -> (String.length h >= 2 && String.length h <= 5) && (String.length t = 4)
  | _ -> false

(** [check_type tl] is true if for the string list [tl] with two elements,
    the first string has only non-numbers, and the second string has only
    numbers. False otherwise. *)
let check_type tl = 
  match tl with
  | h::t::[] ->
    (Str.string_match (Str.regexp "[^0-9]+$") h 0) && (Str.string_match (Str.regexp "[0-9]+$") t 0)
  | _ -> false

(** [is_valid_class tl] is true if the inputted class is a valid format of class
    name. It is false otherwise. *)
let is_valid_class (tl:string list) =
  check_length tl && check_type tl

(** [is_valid_class_st st] is true if the inputted class is more than 3 classes
    and less than 9 classes, which generally satisfies the minimum and maximum
    credits required or available. *)
let is_valid_class_st st = 
  (List.length st.classes_input < 9) && (List.length st.classes_input > 3)

(** [check_sem sem] is true if the inputted string [sem] is either "SP" or "FA".
    Otherwise, it is false. *)
let check_sem sem = 
  sem = "SP" || sem = "FA"

(** [check_num hd] is true if the inputted number [hd] is a 1-2 digit number. 
    Otherwise, it is false. *)
let check_num hd = 
  String.length hd = 4 && (Str.string_match (Str.regexp "[0-9]+$") hd 2)

(** [is_valid_sem tl] is true if both the letter part and the number part of 
    the input [tl] has the correct format. Otherwise, it is false. 
    This will be checked through check_sem and check_num. *)
let is_valid_sem (tl:string list) = 
  match tl with
  | hd::[] -> check_sem ((^) (String.get hd 0 |> Char.escaped) (String.get hd 1 |> Char.escaped)) && check_num hd
  | _ -> false

(** [is_valid_sem_st st] is true if the semester of current state [st] is not an
    empty string. *)
let is_valid_sem_st st = st.semester <> ""

(** [take_sem st tl] is the user attempting to take a semester input [tl] as 
    their answer to the survey question. If the user has not answered the 
    question yet and has an empty string as one's answer, the user will take 
    the content of [tl] as the input. Otherwise, the user will have the same
    previous answer as the input and ignore the incoming one. *)
let take_sem (st:t) tl = 
  if st.semester = "" then
    { st with
      semester = List.hd tl;
    }
  else
    st

(** [delete_sem st tl] is the user attempting to delete the previous input [tl]
    for the semester prompt. If the user is attempting to delete the 
    pre-existing input, then the user will successfully empty out the reponse 
    for this particular question. Otherwise, the input will remain in the state,
    ignoring the incoming delete command. *)
let delete_sem (st:t) tl = 
  if st.semester = List.hd tl then
    { st with
      semester = "";
    }
  else
    st

(** [take_class st tl] is the user attempting to take a class [tl] as their
    answer to the survey question. The string list of one class (ex. ["CS";"3110"])
    is concatenated to the classes_input of [st]. *)
let take_class (st:t) (tl:string list) = 
  { st with
    classes_input = tl :: st.classes_input
  }

(** [delete_class st tl] is the user attempting to delete a class [tl] as their
    answer to the survey question. The classes_input of [st] is a new string 
    list list that has all the elements except [tl]. *)
let delete_class (st:t) (tl:string list) = 
  if List.mem tl st.classes_input then
    { st with
      classes_input = List.filter(fun x -> x <> tl) st.classes_input
    }
  else 
    st 


(** [print_class_helper acc classes] is the string [acc] that concatenates all 
    the class_ids in the string list list[classes]*)
let rec print_class_helper acc classes=
  match classes with 
  | hd::tl -> print_class_helper (String.concat " " hd :: acc) tl
  | _ -> acc

(** [print_sem st] is the string that reveals the current input in the state 
    [st] for semester question prompt. *)
let print_sem = function
    { semester = sem } -> if sem = "" then "Semester not Specified."
    else "Currently selected semester: " ^ sem ^ "\nIf you would like to proceed, type next."

(** [print_class] converts classes_input of [st] into a string with the correct
    format to be printed in the prompt. *)
let print_class = function
    { classes_input = classes } -> if classes = [] then "Currently Entered Class IDs: None"
    else print_class_helper [] classes 
         |> String.concat ", " 
         |> (^) "Currently Entered Class IDs: " 




(* workstyle, sleeping schedule, eating habits 
   - workstyle: cram all classes w/o break or have lots of break in between classes?
   - sleeping schedule: do you prefer morning classes or do you prefer night classes?
   - eating habits: on a scale from 1 to 10 how much is lunch time important to you? *)

(** [prompt_routine st] prompts user to answer routine question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
let rec prompt_routine st = 
  failwith("unimplemented")

(** [prompt_class st] prompts user to answer class question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
let rec prompt_class st = 
  ANSITerminal.(print_string [green] ((print_class st)^"\n"));
  print_endline "Command take: Input the class that you want to enroll in with a space in between the department and the class number. (Ex. 'take CS 3110')";
  print_endline "Command delete: Input the class that you want to delete from your preferred class list. (Ex. 'delete CS 3110')";
  print_endline "Command next: Proceeding to the next question. (Ex. next)";
  print_endline "Command quit: Exit the program. (Ex. quit)";
  print_endline "\nPlease enter a course id that you would like to enroll in. (Commands: 'take', 'delete', 'next', 'quit')\n";
  print_string "> ";
  match parse_class(read_line () |> String.uppercase_ascii) with
  | Quit -> Stdlib.exit 0
  | Next -> if is_valid_class_st st then prompt_routine st 
    else ANSITerminal.(print_string [red] ("You have too little(less than 3) or too much(more than 9) classes to proceed.\n")); prompt_class st
  | Delete tl -> if is_valid_class tl && ((tl |> delete_class st) <> st) then tl |> delete_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a class in the valid format or one of the selected class.\n")); prompt_class st
  | Take tl -> if is_valid_class tl then tl |> take_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid command statement.\n")); prompt_class st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_class st

(** [prompt_semester st] prompts user to answer semester question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
let rec prompt_semester st = 
  ANSITerminal.(print_string [green] ((print_sem st)^"\n"));
  print_endline "Command take: Input the semester that you want to get a schedule for: Abbreviation of the term of the semester with two digit number for year combined. (Ex. 'take SP20')";
  print_endline "Command delete: Delete the selected semester. (Ex. 'delete SP20')";
  print_endline "Command next: Proceeding to the next question. (Ex. next)";
  print_endline "Command quit: Exit the program. (Ex. quit)";
  print_endline "Please enter the semester that you would like to get a schedule for. (Commands: 'take', 'delete', 'next', 'quit')\n";
  print_string "> ";
  match parse_semester(read_line () |> String.uppercase_ascii) with
  | Quit -> Stdlib.exit 0
  | Next -> if is_valid_sem_st st then prompt_class st
    else ANSITerminal.(print_string [red] ("You have not selected a semester to proceed with. \n")); prompt_semester st
  | Delete tl when is_valid_sem tl -> if ((tl |> delete_sem st) <> st) then tl |> delete_sem st |> prompt_semester
    else ANSITerminal.(print_string [red] ("You can only delete the selected semester.\n")); prompt_semester st
  | Delete tl -> ANSITerminal.(print_string [red] ("Please enter the semester in a valid format.\n")); prompt_semester st
  | Take tl when is_valid_sem tl -> if ((tl |> take_sem st) <> st) then  tl |> take_sem st |> prompt_semester
    else ANSITerminal.(print_string [red] ("You have already selected this semester.\n")); prompt_semester st
  | Take tl -> ANSITerminal.(print_string [red] ("Please enter the semester in a valid format.\n")); prompt_semester st
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid command statement.\n")); prompt_semester st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_semester st

=======
>>>>>>> master
