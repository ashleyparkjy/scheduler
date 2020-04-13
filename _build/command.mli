type object_phrase = string list

type command = 
  | Take of object_phrase
  | Delete of object_phrase
  | End
  | Quit

exception Malformed

exception Empty

val parse : string -> command
