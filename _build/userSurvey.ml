open Command
(** type of class id. ex) ["CS";"3110"]*)
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

(* get classes from string list list to string assoc list. *)
let get_classes st = 
  let rec to_assoc_list list acc =
    match list with
    | [] -> acc
    | h::t when List.length h = 2 -> to_assoc_list t ((List.nth h 0, List.nth h 1)::acc)
    | _ -> failwith "error" in
  to_assoc_list st.classes_input []


let check_length tl = 
  match tl with
  | h::t::[] -> (String.length h >= 2 && String.length h <= 5) && (String.length t = 4)
  | _ -> false

let check_type tl = 
  match tl with
  | h::t::[] ->
    not(Str.string_match (Str.regexp "[0-9]+$") h 0) && (Str.string_match (Str.regexp "[0-9]+$") t 0)
  | _ -> false

(* check valid input - 4 or 5 digit *)

let is_valid_class (tl:string list) =
  check_length tl && check_type tl

let is_valid_class_st st = 
  (List.length st.classes_input < 9) && (List.length st.classes_input > 3)

let check_sem sem = 
  sem = "SP" || sem = "FA"

let check_num hd = 
  (Str.string_match (Str.regexp "[0-9]+$") hd 2)

let is_valid_sem (tl:string list) = 
  match tl with
  | hd::[] -> check_sem ((^) (String.get hd 0 |> Char.escaped) (String.get hd 1 |> Char.escaped)) && check_num hd
  | _ -> false

let is_valid_sem_st st = st.semester <> ""

(* Take sem *)
let take_sem (st:t) tl = 
  if st.semester = "" then
    { st with
      semester = List.hd tl;
    }
  else
    st

(* Delete sem *)
let delete_sem (st:t) tl = 
  if st.semester = List.hd tl then
    { st with
      semester = "";
    }
  else
    st

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
  else 
    st



let rec print_class_helper acc classes=
  match classes with 
  | hd::tl -> print_class_helper (String.concat " " hd :: acc) tl
  | _ -> acc

let print_sem = function
    { semester = sem } -> if sem = "" then "Semester not Specified."
    else "Currently selected semester: " ^ sem ^ "\nIf you would like to proceed, type next."

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
  | Delete tl -> if is_valid_class tl then tl |> delete_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | Take tl -> if is_valid_class tl then tl |> take_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid command statement.\n")); prompt_class st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_class st

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
  | Delete tl -> if is_valid_sem tl then  tl |> delete_sem st |> prompt_semester
    else ANSITerminal.(print_string [red] ("Please enter the semester in a valid format.\n")); prompt_semester st
  | Take tl -> if is_valid_sem tl then  tl |> take_sem st |> prompt_semester
    else ANSITerminal.(print_string [red] ("Please enter the semester in a valid format.\n")); prompt_semester st
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid command statement.\n")); prompt_semester st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_semester st

