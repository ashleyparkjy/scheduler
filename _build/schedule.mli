(** 
   Representation of a schedule.

   This module represents the configuration of classes stored in a schedule.
*)

(** The abstract type of values representing a schedule. *)
type t

(** The type of event, representing one obligation with a set start and end
    time. *)
type event = {
  course_name: string;
  description: string;
  course_id: Classes.course_id;
  credits: int;
  subject: string;
  catalog_number: int;
  event_type: string;
  section_id: Classes.section_id;
  section_number: string;
  meeting_id: Classes.meeting_id;
  start_time: Classes.time;
  end_time: Classes.time;
  instructors: string list;
  facility: string;
  building: string;
}

(** [add_section s c r t] is the schedule [t] with the addition of section [s]
    of class [c] in roster [r].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id. *)
val add_section : Classes.section_id -> Classes.course_id -> Classes.t -> t -> t

(** [empty] is the representation of the empty schedule. *)
val empty : t

(** [is_empty t] is true if [t] is empty. Else, false. *)
val is_empty : t -> bool

(** [size t] is the number of sections in schedule [t]. *)
val size: t -> int

(** [peak t] gives an element of [t]. *)
val peak: t-> event

(** [get_events t] is the list of events that represents all events in
    schedule [t]. *)
val get_events: t -> event list

(** [get_events t] is the list of events that represents all events in
    schedule [t] on Monday. *)
val get_monday: t -> event list

(** [get_events t] is the list of events that represents all events in
    schedule [t] on Tuesday. *)
val get_tuesday: t -> event list

(** [get_events t] is the list of events that represents all events in
    schedule [t] on Wednesday. *)
val get_wednesday: t -> event list

(** [get_events t] is the list of events that represents all events in
    schedule [t] on Thursday. *)
val get_thursday: t -> event list

(** [get_events t] is the list of events that represents all events in
    schedule [t] on Friday. *)
val get_friday: t -> event list