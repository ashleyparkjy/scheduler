

type object_phrase = string list

type command = 
  | Take of object_phrase
  | Delete of object_phrase
  | Next
  | Quit

exception Malformed

exception Empty

(** [str_list string] is a string list of non-space characters with empty 
    string elements eliminated from [string]. 
    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).*)
let str_list string =
  let list = String.split_on_char ' ' string in 
  let rec helper list acc =
    match list with
    | [] -> List.rev acc
    | "" :: t -> helper t acc
    | h :: t -> helper t (h::acc) in 
  helper list []


let parse_class str =
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "QUIT" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "NEXT" -> if List.length t = 0 then Next else raise Malformed 
  | h :: t when h = "DELETE" -> if List.length t = 2 then Delete t else raise Malformed
  | h :: t when h = "TAKE" -> if List.length t = 2 then Take t else raise Malformed
  | _ -> raise Malformed

let parse_semester str = 
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "QUIT" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "NEXT" -> if List.length t = 0 then Next else raise Malformed 
  | h :: t when h = "DELETE" -> if List.length t = 1 then Delete t else raise Malformed
  | h :: t when h = "TAKE" -> if List.length t = 1 then Take t else raise Malformed
  | _ -> raise Malformed

let parse_class_time str = 
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "QUIT" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "NEXT" -> if List.length t = 0 then Next else raise Malformed 
  | h :: t when h = "DELETE" -> if List.length t = 0 then Delete t else raise Malformed
  | h :: t when h = "TAKE" -> if List.length t = 2 then Take t else raise Malformed
  | _ -> raise Malformed

let parse_YN str = 
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "QUIT" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "NEXT" -> if List.length t = 0 then Next else raise Malformed 
  | h :: t when h = "DELETE" -> if List.length t = 0 then Delete t else raise Malformed
  | h :: t when h = "TAKE" -> if List.length t = 1 then Take t else raise Malformed
  | _ -> raise Malformed