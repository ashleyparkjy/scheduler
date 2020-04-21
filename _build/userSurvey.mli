type t

type class_id

val init_state : t

val get_semester : t -> string

val get_classes : t -> (string * string) list 

val take_sem : t -> string list -> t

val take_class : t -> string list -> t

val prompt_class : t -> unit

val prompt_semester : t -> unit

