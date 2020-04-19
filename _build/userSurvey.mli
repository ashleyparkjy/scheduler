type t

type class_id

val init_state : t

val take_class: t -> string list -> t

val prompt_class : t -> unit