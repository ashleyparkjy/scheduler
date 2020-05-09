(** 
   Ranking Algorithm that rules out invalid schedules and ranks schedules 
   based on user preferences. 
*)

(** The type of event, representing one obligation with a set start and end
    time. *)
type event = Schedule.event

(** The abstract type of values representing a schedule. *)
type t = Schedule.t

(** The type of comparable event, storing event, start time, and end time. *)
type comparable_event

(** The type of valid schedule. *)
type t_valid

(** [get_start_time cmp_event] is the start time of [cmp_event] in minutes. *)
val get_start_time : comparable_event -> int

(** [time_to_min time] converts [time] to minutes in int from Classes record type. *)
val time_to_min : Classes.time -> int 

(** [comparable_list acc t] is a comparable_event list from the schedule [t]. *)
val comparable_list : comparable_event list -> event list -> comparable_event list

(** [sort_start_time cmp_list] is a sorted comparable_event list from the least 
    to the greatest start time values. *)
val sort_start_time : comparable_event list -> comparable_event list

(** [is_duplicate cmp_list] is true if [cmp_list] contains more than one event
    with the same start time. Otherwise, false. *)
val is_duplicate : comparable_event list -> bool

(** [end_time_compare cmp_list] is false if [cmp_list] contains at least one
    event that overlaps with another event. Otherwise, true. *)
val end_time_compare: comparable_event list -> bool

(** [check_day_schedule event_list] is true if [event_list] satisfies both
    is_duplicate and end_time_compare, which means that the event list does not
    have any overlapping class times. *)
val check_day_schedule: event list -> bool

(** [filter_valid_schedule acc t_list] is the list of schedules [acc] filtered
    list of list of schedules [t_list] with the schedules that do not have 
    overlapping class time. *)
val filter_valid_schedule: t list -> t list -> t list