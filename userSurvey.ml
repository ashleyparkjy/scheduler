open Command

type class_id = string list

type t = {
  classes_input: class_id list
}

exception Malformed

let init_state = 
  {
    classes_input = []
  }

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


(* Take class *)
let take_class (st:t) (tl:string list) = 
  {
    classes_input = tl :: st.classes_input
  }

(* Delete class *)
let delete_class (st:t) (tl:string list) = 
  if List.mem tl st.classes_input then
    {
      classes_input = List.filter(fun x -> x <> tl) st.classes_input
    }
  else st

(* Drop *)


let rec print_class_helper acc classes=
  match classes with 
  | hd::tl -> print_class_helper (String.concat "" hd :: acc) tl
  | _ -> acc

(* class list print *)
let print_class = function
    {classes_input = classes} -> if classes = [] then "Currently Entered Class IDs: None"
    else print_class_helper [] classes 
         |> String.concat ", " 
         |> (^) "Currently Entered Class IDs: " 

let rec question2_prompt st = 
  failwith("unimplemented")

(* class input *)
let rec prompt_class st = 
  ANSITerminal.(print_string [green] ((print_class st)^"\n"));
  print_endline "Please enter a course id that you would like to enroll in. (Commands: 'take', 'delete', 'quit', 'end')\n";
  print_endline "Command take: Input the class that you want to enroll in with a space in between the department and the class number. (Ex. 'take CS 3110')";
  print_endline "Command delete: Input the class that you want to delete from your preferred class list. (Ex. 'delete CS 3110')";
  print_endline "Command end: Proceeding to the next question. (Ex. end)";
  print_endline "Command quit: Exit the program. (Ex. quit)";
  print_string "> ";
  match parse(read_line ()) with
  | Quit -> Stdlib.exit 0
  | End -> question2_prompt st
  | Delete tl -> if is_valid_class tl then tl |> delete_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | Take tl -> if is_valid_class tl then tl |> take_class st |> prompt_class 
    else ANSITerminal.(print_string [red] ("Please enter in a valid format.\n")); prompt_class st
  | exception Malformed -> ANSITerminal.(print_string [red] ("Please use a valid format.\n")); prompt_class st
  | exception Empty -> ANSITerminal.(print_string [red] ("Your input was empty.\n")); prompt_class st



