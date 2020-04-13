type class_id = string

type t = {
  classes: class_id list
}

let init_state = 
  {
    classes = []
  }

(* check valid input -  *)
let class_valid (st:t) = 
  failwith("Unimplemented")



(* Take *)
let class_take (st:t) (crs:class_id) = 
  {
    classes = (crs :: st.classes)
  }

(* Drop *)

(* class list print *)
let class_print = function
    {classes = classes} -> 
    if classes =[] then print_endline "Currently Entered Class IDs: None"
    else classes 
         |> String.concat ", " 
         |> (^) "Currently Entered Class IDs: " 
         |> print_endline

let rec question2_prompt st = 
  failwith("unimplemented")

(* class input *)
let rec class_prompt st = 
  class_print st;
  print_endline "Please enter a course id that you would like to enroll in.\n";
  print_string  "> ";
  match Command.parse(read_line ()) with
  | "end" -> question2_prompt st
  | s -> class_take st s  |> class_prompt 



