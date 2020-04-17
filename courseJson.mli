(** Produces a JSON file with relevant course data from the Cornell class
    roster API, using user-inputted specifications. *)

val body : string -> string Lwt.t
val runner : string -> unit
