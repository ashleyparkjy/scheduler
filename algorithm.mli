(** 
   Ranking Algorithm that rules out invalid schedules and ranks schedules 
   based on user preferences. 
*)

(** The type of event, representing one obligation with a set start and end
    time. *)
type event = Schedule.event

(** The abstract type of values representing a schedule. *)
type t = Schedule.t

type ranked_schedule = (t * float) list

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

(** [day_class_time acc event_l] is the total time of classes for the specified day schedule
    [event_l] in int. *)
val day_class_time: int -> event list -> int

(** [score_lunch t] is the sum of individual lunch time score of each day in float.
    The score of each day is 0.2 if the schedule has lunch time between 11:00
    and 13:00. Otherwise, 0.

    Requires: [classtime] is a tuple of int, which contains the starting times
    of first and last class. 
    Requires: [t] is Schedule.t. *)
val score_lunch: t -> float

(** [score_spread t] is the standard deviation of class times over the week in 
    float. The standard deviation is low if the class schedule is evenly 
    spreaded out across 5 days. 
    Requires: [t] is Schedule.t. *)
val score_spread: t -> float

(** [score_classtime classtime t] is the sum of individual class time score of 
    each day in float.
    Requires: [classtime] is a tuple of int, which contains start time and 
    end time of classes. 
    Requires: [t] is Schedule.t. *)
val score_classtime: (int*int) -> t -> float

(** [schedule_score output t] is the score of given schedule [t] in float. The 
    total score consists of three subcategory scores, which are spread score, lunch
    score, and class time score. 

    For spread score, if user answered "Y" and prefers classes spreaded out,
    it is one minus standard deviation of class times of each day. If user 
    answered "N" and prefers classes clustered, the score is standard deviation 
    itself. 

    For lunch score, if user anwered "Y" and is not flexible with lunch 
    time, then the score is 0.2 * number of days in which lunch time is preserved
    between 11AM and 1PM. If user answered "N" and is flexible with lunch
    time, then the score is just 1.

    For class time score, it depends on the user preference on start time and
    end time of classes. The score is 0.2 * number of days in which the 
    schedule satisfies the requirements. Note that if a day's schedule
    satisfies only one of the requirements, the score is 0.1 for that day.

    Requires: [output] is the final output from UserSurvey of type t_output.
    Requires: [t] is Schedule.t. *)
val schedule_score: UserSurvey.t_output -> t -> float

(** [rank_schedule n acc output t_list] ranks all schedules in decreasing order
    of their scores and then gives a list of schedules that have top [n]
    scores. If the number of original schedules created is less than [n], then this
    method returns the original schedules in the order of high to low scores. *)
val rank_schedule: int -> ranked_schedule -> UserSurvey.t_output -> t list -> ranked_schedule

(** [delete_dups s1 s2] is the list of schedules from s1 and s2 with no duplicates. *)
val delete_dups:  Schedule.t list -> Schedule.t list -> Schedule.t list