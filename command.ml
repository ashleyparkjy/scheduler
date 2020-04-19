type object_phrase = string list

type command = 
  | Take of object_phrase
  | Delete of object_phrase
  | End
  | Quit

exception Malformed

exception Empty

let str_list string =
  let list = String.split_on_char ' ' string in 
  let rec helper list acc =
    match list with
    | [] -> List.rev acc
    | "" :: t -> helper t acc
    | h :: t -> helper t (h::acc) in 
  helper list []


let parse str =
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "quit" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "end" -> if List.length t = 0 then End else raise Malformed 
  | h :: t when h = "delete" -> if List.length t = 2 then Delete t else raise Malformed
  | h :: t when h = "take" -> if List.length t = 2 then Take t else raise Malformed
  | _ -> raise Malformed