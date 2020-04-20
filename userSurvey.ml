open Command

type class_id = string list

type t = {
  semester: string;
  classes_input: class_id list
}

let init_state = 
  {
    semester = "";
    classes_input = []
  }

(* get semester *)
let get_semester st = 
  st.semester

(* get classes in string tuple list *)
(* let get_classes st = 
   let rec to_assoc_list st acc =
    match st.classes_input with
    | [] -> []
    | h::t when List.length h = 2 -> (List.hd h, List.tl h) :: [] 
    | _ -> in 
   to_assoc_list st [] *)


let check_length tl = 
  match tl with
  | h::t::[] -> (String.length h >= 2 && String.length h <= 5) && (String.length t = 4)
  | _ -> false

let check_type tl = 
  match tl with
  | h::[t] ->
    not(Str.string_match (Str.regexp "[0-9]+$") h 0) && (Str.string_match (Str.regexp "[0-9]+$") t 0)
  | _ -> false

(* check valid input - 4 or 5 digit *)

let is_valid_class (tl:string list) =
  check_length tl && check_type tl

(* Take sem *)
let take_sem tl (st:t) = 
  if st.semester = "" then
    { st with
      semester = tl;
    }
  else
    st

(* Delete sem *)
let delete_sem tl (st:t) = 
  if st.semester = tl then
    { st with
      semester = "";
    }
  else
    st

(* Take class *)
let take_class (st:t) (tl:string list) = 
  { st with
    classes_input = tl :: st.classes_input
  }

(* Delete class *)
let delete_class (st:t) (tl:string list) = 
  if List.mem tl st.classes_input then
    { st with
      classes_input = List.filter(fun x -> x <> tl) st.classes_input
    }

let rec print_class_helper acc classes=
  match classes with 
  | hd::tl -> print_class_helper (String.concat "" hd :: acc) tl
  | _ -> acc

(* semester print *)
let print_sem = function
    { semester = sem } -> if sem = "" then "Semester not Specified."
    else "Currently selected semester: " ^ sem ^ "\n Would you like to proceed?"

(* class list print *)
let print_class = function
    { classes_input = classes } -> if classes = [] then "Currently Entered Class IDs: None"
    else print_class_helper [] classes 
         |> String.concat ", " 
         |> (^) "Currently Entered Class IDs: " 

let rec prompt_routine st = 
  failwith("unimplemented")

(* class input *)
let rec prompt_class st = 
  ANSITerminal.(print_string [green] ((print_class st)^"\n"));
  print_endline "Please enter a course id that you would like to enroll in. (Commands: 'take', 'delete', 'end', 'quit')\n";
  print_endline "Command take: Input the class that you want to enroll in with a space in between the department and the class number. (Ex. 'take CS 3110')";
  print_endline "Command delete: Input the class that you want to delete from your preferred class list. (Ex. 'delete CS 3110')";
  print_endline "Command end: Proceeding to the next question. (Ex. end)";
  print_endline "Command quit: Exit the program. (Ex. quit)";
  print_string "> ";
  match parse_class(read_line ()) with
  | Quit -> Stdlib.exit 0
  | End -> question2_routine st
  | Delete tl -> if is_valid_class tl then tl |> delete_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | Take tl -> if is_valid_class tl then tl |> take_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid format.\n")); prompt_class st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_class st

let rec prompt_semester st = 
  ANSITerminal.(print_string [green] ((print_sem st)^"\n"));
  print_endline "Please enter the semester that you would like to get a 
  schedule for. (Commands: 'take', 'delete', 'end', 'quit')\n";
  print_string "> ";
  match parse(read_line ()) with
  | Quit -> Stdlib.exit 0
  | End -> prompt_class st
  | Delete tl -> tl |> delete_sem tl |> prompt_semester
  | Take tl -> tl |> take_sem tl |> prompt_semester
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid format.\n")); prompt_semester st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_semester st

