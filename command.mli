(**
   Parsing of user commands.
*)

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
val parse_class : string -> command

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
val parse_semester : string -> command

val parse_class_time : string -> command

val parse_YN: string -> command