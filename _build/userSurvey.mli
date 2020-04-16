type t

type class_id

val init_state : t

val get_class : t -> string list

val take_class : t -> string list -> t

val prompt_class : t -> unit