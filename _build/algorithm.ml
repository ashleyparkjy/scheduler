

type event = Schedule.event

type t = Schedule.t

type comparable_event = {
  event_object: event;
  start_time_min: int;
  end_time_min: int; 
}

type t_valid = {
  events: t;
  start_time_valid: bool;
}

let get_start_time cmp_event = 
  cmp_event.start_time_min

let time_to_min (time:Classes.time) = (time.hr*60) + time.min

(** [to_comparable_event event] converts [event] of type Schedule.event to 
    type comparable_event. The output of this function is used to compare the
    event's time and duration with other events to determine if there is a time
    conflict. *)
let to_comparable_event event = {
  event_object = event;
  start_time_min = event.start_time |> time_to_min;
  end_time_min = event.end_time |> time_to_min;
}

let rec comparable_list acc t =
  match t with
  | hd::tl -> comparable_list ((to_comparable_event hd) :: acc) tl
  | _ -> acc

(** [compare a b] is -1 when start time of event [a] is less than that of event
    [b], 0 when equal, 1 when greater. *)
let compare a b =
  if a.start_time_min < b.start_time_min then -1
  else if a.start_time_min > b.start_time_min then 1
  else 0

let sort_start_time cmp_list = 
  List.sort compare cmp_list

(** [start_time_lst acc cmp_list] is an int list of start time values of event
    in [cmp_list]. *)
let rec start_time_lst acc cmp_list =
  match cmp_list with 
  | [] -> acc
  | hd::tl -> start_time_lst (hd.start_time_min :: acc) tl

let is_duplicate cmp_list = 
  let lst = cmp_list |> start_time_lst [] in
  let rec helper list =
    match list with
    | [] -> true
    | hd::tl -> if List.mem hd tl then false else helper tl in
  helper lst

let rec end_time_compare cmp_list =
  match cmp_list with
  | [] -> true
  | hd2::[] -> true
  | hd::hd2::tl -> if (hd.end_time_min < hd2.start_time_min) then end_time_compare (hd2::tl) else false

let check_day_schedule event_list = 
  let cmp_list = event_list |> comparable_list [] |> sort_start_time in
  is_duplicate cmp_list && end_time_compare cmp_list

(** [check_valid_schedule t] is true if the schedule [t] does not have overlapping
    class times on each day of the week. Otherwise, false.*)
let check_valid_schedule t = 
  t |> Schedule.get_monday |> check_day_schedule &&
  t |> Schedule.get_tuesday |> check_day_schedule &&
  t |> Schedule.get_wednesday |> check_day_schedule &&
  t |> Schedule.get_thursday|> check_day_schedule &&
  t |> Schedule.get_friday |> check_day_schedule

let rec filter_valid_schedule acc t_list = 
  match t_list with
  | [] -> acc
  | hd::tl -> if check_valid_schedule hd 
    then filter_valid_schedule (hd::acc) tl 
    else filter_valid_schedule acc tl





(** [day_class_time acc event_l] is the total time of classes for the specified day schedule
    [event_l] in int. *)
let rec day_class_time acc event_l =
  match event_l with
  | hd::tl -> day_class_time ((hd |> to_comparable_event).end_time_min - (hd |> to_comparable_event).start_time_min + acc) tl
  | [] -> acc

(** [score_spread t] is the standard deviation of class times over the week in 
    float. The standard deviation is low if the class schedule is evenly 
    spreaded out across 5 days. 
    Requires: [t] is Schedule.t. *)
let score_spread t = 
  let m_class_time = t |> Schedule.get_monday |> day_class_time 0 in
  let t_class_time = t |> Schedule.get_tuesday |> day_class_time 0 in
  let w_class_time = t |> Schedule.get_wednesday |> day_class_time 0 in
  let th_class_time = t |> Schedule.get_thursday |> day_class_time 0 in
  let f_class_time = t |> Schedule.get_friday|> day_class_time 0 in
  let mean = 
    float_of_int(m_class_time + t_class_time + w_class_time + th_class_time + f_class_time) /. 5. in
  let time_square t = ((float_of_int t) -. mean) ** 2. in
  let standard_deviation =  
    sqrt(((m_class_time |> time_square) +. 
          (t_class_time |> time_square) +. 
          (w_class_time |> time_square) +. 
          (th_class_time |> time_square) +. 
          (f_class_time |> time_square)) /. 5.) in
  standard_deviation


(* acc = 120
   1) starttime - before 11, endtime - after 11 (10:10 - 11:05)
   2) starttime - after 11, endtime - before 1 (11:15 - 12:10)
   3) starttime - before 1, endtime after 1 (12:10 - 1:05)
   if remaining acc is >= 60 then we call that it has valid lunch time between 11 to 1 
   - *)


(** [check_lunch_time even_l] is bool checking whether the day schedule [even_l]
    has at least 60 minutes lunch time in between 11:00 and 13:00. True if it does,
    else false. *)
let check_lunch_time event_l = 
  let rec lunch_duration acc event_l = 
    match event_l with
    | hd::tl -> if (hd|>to_comparable_event).start_time_min < 660 && (hd|>to_comparable_event).end_time_min > 660 
      then lunch_duration (acc-((hd|>to_comparable_event).end_time_min - 660)) tl
      else if (hd|>to_comparable_event).start_time_min >= 660 && (hd|>to_comparable_event).end_time_min <= 780
      then lunch_duration (acc-((hd|>to_comparable_event).end_time_min - (hd|>to_comparable_event).start_time_min)) tl 
      else lunch_duration (780 - (hd|>to_comparable_event).start_time_min) tl
    | [] -> acc in
  (lunch_duration 120 event_l) >= 60

(** [score_lunch t] is the sum of individual lunch time score of each day in float.
    The score of each day is 0.2 if the schedule has lunch time between 11:00
    and 13:00. Otherwise, 0.

    Requires: [classtime] is a tuple of int, which contains the starting times
    of first and last class. 
    Requires: [t] is Schedule.t. *)
let rec score_lunch t =
  let m_lunch_score = if t |> Schedule.get_monday |> check_lunch_time then 0.2 else 0. in
  let t_lunch_score = if t |> Schedule.get_tuesday |> check_lunch_time then 0.2 else 0. in
  let w_lunch_score = if t |> Schedule.get_wednesday |> check_lunch_time then 0.2 else 0. in
  let th_lunch_score = if t |> Schedule.get_thursday |> check_lunch_time then 0.2 else 0. in
  let f_lunch_score = if t |> Schedule.get_friday |> check_lunch_time then 0.2 else 0. in
  m_lunch_score+.t_lunch_score+.w_lunch_score+.th_lunch_score+.f_lunch_score

(** [classtime_cond classtime hd tl] checks if the first class start time [hd] 
    and the last time start time [tl] meet the requirements of user preference
    in [classtime].
    The score is 0. if both requirements are not met. 
    The score is 0.1 if only one is met. 
    The score is 0.2 if both are met. *)
let classtime_cond classtime hd tl =
  if (hd|>to_comparable_event).start_time_min >= (fst classtime) 
  && (tl|>to_comparable_event).end_time_min <= (snd classtime)
  then 0.2
  else if (hd|>to_comparable_event).start_time_min >= (fst classtime) 
       || (tl|>to_comparable_event).end_time_min <= (snd classtime) then 0.1
  else 0.

(** [check_classtime classtime event_l] is the class time score of the given
    day's schedule in float. If the schedule [event_l] is empty, the score is 
    0.2. Otherwise, call [classtime_cond] to calculate the score based on the
    requirements [classtime]. *)
let rec check_classtime classtime event_l =  
  match event_l with
  | [] -> 0.2
  | hd::[] -> classtime_cond classtime hd hd
  | hd::tl -> classtime_cond classtime hd (tl|>List.rev|>List.hd)

(** [score_classtime classtime t] is the sum of individual class time score of 
    each day in float.
    Required: [classtime] is a tuple of int, which contains the starting times
    of first and last class. 
    Required: [t] is Schedule.t. *)
let score_classtime classtime (t:Schedule.t) = 
  let m_time_score = t |> Schedule.get_monday |> check_classtime classtime in 
  let t_time_score = t |> Schedule.get_tuesday |> check_classtime classtime in 
  let w_time_score = t |> Schedule.get_wednesday |> check_classtime classtime in 
  let th_time_score = t |> Schedule.get_thursday |> check_classtime classtime in 
  let f_time_score = t |> Schedule.get_friday |> check_classtime classtime in 
  m_time_score+.t_time_score+.w_time_score+.th_time_score+.f_time_score


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

    For class time score, it depends on the user preference on starting times of
    first and last classes. The score is 0.2 * number of days in which the 
    schedule satisfies the requirements. Note that if a day's schedule
    satisfies only one of the requirements, the score is 0.1 for that day.

    Requires: [output] is the final output from UserSurvey of type t_output.
    Requires: [t] is Schedule.t. *)
let schedule_score (output:UserSurvey.t_output) (t:Schedule.t) = 
  let spread = if (output.spread_output = "N") then score_spread t else (1. -. score_spread t) in 
  let lunch = if (output.lunch_output = "N") then 1. else score_lunch t in 
  let classtime = score_classtime output.classtime t in 
  spread +. lunch +. classtime

(** TODO *)
let rank_schedule (output:UserSurvey.t_output) (t_list:Schedule.t list) =
  failwith "unimplemented"




