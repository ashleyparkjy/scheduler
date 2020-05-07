(** 
   Representation of schedule visualization.

   This module represents the visualized list of schedules in the terminal.
*)

(** The abstract type of values representing a schedule. *)
type t = Schedule.t list

(** [visualize t] prints the list of schedules t to the terminal and returns
    unit. *)
val visualize : t -> unit