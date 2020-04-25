(** 
   Ranking Algorithm that rules out invalid schedules and ranks schedules 
   based on user preferences. 
*)

type event = Schedule.event

type t = Schedule.t

type comparable_event

type t_valid

val get_start_time : comparable_event -> int

val time_to_min : Classes.time -> int 

val comparable_list : comparable_event list -> event list -> comparable_event list

val sort_start_time : comparable_event list -> comparable_event list

val is_duplicate : comparable_event list -> bool

val end_time_compare: comparable_event list -> bool

val check_day_schedule: event list -> bool

val filter_valid_schedule: t list -> t list -> t list