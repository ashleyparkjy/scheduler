type t

type class_id

val init_state : t

val class_take : t -> class_id -> t

val class_prompt : t -> unit