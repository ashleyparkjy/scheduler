
(** The type [object_phrase] represents the object phrase that can be part of a
    user command. Each element of the list represents a word of the object phrase
    without any spaces. The list is in the same order as the words in the 
    original user command. *)
type object_phrase = string list

(** The type [command] represents a user command that is decomposed
    into a verb and possibly an object phrase. *)
type command = 
  | Take of object_phrase
  | Delete of object_phrase
  | Next
  | Quit

(** Raised when a malformed command is encountered. *)
exception Malformed

(** Raised when an empty command is parsed. *)
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


(** [parse_class str] parses a user's input into a [command], as follows. The
    first word of [str] becomes the verb. The rest of the words, if any, become
    the object phrase. 
    Examples:
    - [parse "TAKE CS 3110"] is [Take ["CS"; "3110"]] 
    - [parse "QUIT"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] is [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command is {i malformed} 
    if the verb is neither of "QUIT", "NEXT", "DELETE", "TAKE",
    or if the verb is "QUIT" and there is a non-empty object phrase, 
    or if the verb is "NEXT" and there is a non-empty object phrase,
    or if the verb is "DELETE" and the length of object phrase is not 2,
    or if the verb is "TAKE" and the length of object phrase is not 2.
*)
let parse_class str =
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "QUIT" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "NEXT" -> if List.length t = 0 then Next else raise Malformed 
  | h :: t when h = "DELETE" -> if List.length t = 2 then Delete t else raise Malformed
  | h :: t when h = "TAKE" -> if List.length t = 2 then Take t else raise Malformed
  | _ -> raise Malformed

(** [parse_semester str] parses a user's input into a [command], as follows. The
    first word of [str] becomes the verb. The rest of the words, if any, become
    the object phrase. 
    Examples:
    - [parse "TAKE CS 3110"] is [Take ["CS"; "3110"]] 
    - [parse "QUIT"] is [Quit]. 

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] is [str] is the empty string or contains only spaces. 

    Raises: [Malformed] if the command is malformed. A command is {i malformed} 
    if the verb is neither of "QUIT", "NEXT", "DELETE", "TAKE",
    or if the verb is "QUIT" and there is a non-empty object phrase, 
    or if the verb is "NEXT" and there is a non-empty object phrase,
    or if the verb is "DELETE" and the length of object phrase is not 1,
    or if the verb is "TAKE" and the length of object phrase is not 1.
*)
let parse_semester str = 
  str |> str_list |> function
  | [] -> raise Empty
  | h :: t when h = "QUIT" -> if List.length t = 0 then Quit else raise Malformed
  | h :: t when h = "NEXT" -> if List.length t = 0 then Next else raise Malformed 
  | h :: t when h = "DELETE" -> if List.length t = 1 then Delete t else raise Malformed
  | h :: t when h = "TAKE" -> if List.length t = 1 then Take t else raise Malformed
  | _ -> raise Malformed
