type object_phrase = string list

type command = 
  | Take of object_phrase
  | Delete of object_phrase
  | Next
  | Quit

exception Malformed

exception Empty

val parse_class : string -> command

val parse_semester : string -> command
