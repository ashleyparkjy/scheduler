open Command

type class_id = string

type t = {
  classes: class_id list
}

let init_state = 
  {
    classes = []
  }

(* check valid input - 4 or 5 digit *)
let is_valid_class (tl:string list) = 
  failwith("Unimplemented")



(* Take class *)
let take_class (st:t) (tl:string list) = 
  {
    classes = ((String.concat "" tl) :: st.classes)
  }

(* Delete class *)
let delete_class (st:t) (tl:string list) = 
  failwith("Unimplemented")

(* Drop *)

(* class list print *)
let print_class = function
    {classes = classes} -> 
    if classes =[] then print_endline "Currently Entered Class IDs: None"
    else classes 
         |> String.concat ", " 
         |> (^) "Currently Entered Class IDs: " 
         |> print_endline

let rec question2_prompt st = 
  failwith("unimplemented")

(* class input *)
let rec prompt_class st = 
  print_class st;
  print_endline "Please enter a course id that you would like to enroll in.\n";
  print_string  "> ";
  match parse(read_line ()) with
  | Quit -> Stdlib.exit 0
  | End -> question2_prompt st
  | Delete tl -> if is_valid_class tl then tl |> delete_class st |> prompt_class 
    else print_endline "Please enter a valid class number."
  | Take tl -> if is_valid_class tl then tl |> take_class st |> prompt_class 
    else print_endline "Please enter a valid class number."



