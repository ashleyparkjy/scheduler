(** 
   Representation of a schedule.

   This module represents the configuration of classes stored in a schedule.
*)

(** The abstract type of values representing a schedule. *)

type t

val generate_schedules : Classes.t -> Schedule.t list