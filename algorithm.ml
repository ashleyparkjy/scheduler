
type event = Schedule.event

type t = Schedule.t

type comparable_event = {
  event_object: event;
  start_time_min: int;
  end_time_min: int; 
}

(** Representation that is [comparable_event] without the event. *)
type comparable_event_bare = {
  days: Classes.day list;
  start_time_min2: int;
  end_time_min2: int; 
}

type t_valid = {
  events: t;
  start_time_valid: bool;
}

type ranked_schedule = (Schedule.t * float) list

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

(** [to_bare_comparable_event event] converts [event] of type Schedule.event to 
    type comparable_event_bare. *)
let to_bare_comparable_event (event : Schedule.event) : comparable_event_bare = {
  days = event.days |> List.sort_uniq compare;
  start_time_min2 = event.start_time |> time_to_min;
  end_time_min2 = event.end_time |> time_to_min;
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

let rec day_class_time acc event_l =
  match event_l with
  | [] -> acc
  | hd::tl -> day_class_time ((hd |> to_comparable_event).end_time_min - (hd |> to_comparable_event).start_time_min + acc) tl

let score_spread t = 
  let m_class_time = t |> Schedule.get_monday |> day_class_time 0 in
  let t_class_time = t |> Schedule.get_tuesday |> day_class_time 0 in
  let w_class_time = t |> Schedule.get_wednesday |> day_class_time 0 in
  let th_class_time = t |> Schedule.get_thursday |> day_class_time 0 in
  let f_class_time = t |> Schedule.get_friday|> day_class_time 0 in
  let mean = 
    float_of_int(m_class_time + t_class_time + w_class_time + th_class_time + f_class_time) /. 5. in
  let time_square t = (t -. mean) ** 2. in
  let standard_deviation =  
    sqrt(((m_class_time |> float_of_int |> time_square) +. 
          (t_class_time |> float_of_int|> time_square) +. 
          (w_class_time |> float_of_int|> time_square) +. 
          (th_class_time |> float_of_int|> time_square) +. 
          (f_class_time |> float_of_int|> time_square)) /. 5.) in
  let max_std = 
    sqrt(((mean *. 5.)|> time_square) /. 5.) in
  standard_deviation /. max_std


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
    | [] -> acc
    | hd::tl -> if (hd|>to_comparable_event).start_time_min < 660 && (hd|>to_comparable_event).end_time_min > 660 
      then lunch_duration (acc-((hd|>to_comparable_event).end_time_min - 660)) tl
      else if (hd|>to_comparable_event).start_time_min >= 660 && (hd|>to_comparable_event).end_time_min <= 780
      then lunch_duration (acc-((hd|>to_comparable_event).end_time_min - (hd|>to_comparable_event).start_time_min)) tl 
      else if (hd|>to_comparable_event).start_time_min <= 780 && (hd|>to_comparable_event).end_time_min >= 780
      then lunch_duration (acc - (780 - (hd|>to_comparable_event).start_time_min)) tl 
      else lunch_duration acc tl in
  (lunch_duration 120 event_l) >= 60

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
  if hd.start_time_min >= (fst classtime) 
  && tl.end_time_min <= (snd classtime)
  then 0.2
  else if hd.start_time_min >= (fst classtime) 
       || tl.end_time_min <= (snd classtime) then 0.1
  else 0.

(** [check_classtime classtime event_l] is the class time score of the given
    day's schedule in float. If the schedule [event_l] is empty, the score is 
    0.2. Otherwise, call [classtime_cond] to calculate the score based on the
    requirements [classtime]. *)
let check_classtime classtime event_l =  
  match event_l |> comparable_list [] |> sort_start_time with
  | [] -> 0.2
  | hd::[] -> classtime_cond classtime hd hd
  | hd::tl -> classtime_cond classtime hd (tl|>List.rev|>List.hd)

let score_classtime classtime (t:Schedule.t) = 
  let m_time_score = t |> Schedule.get_monday |> check_classtime classtime in 
  let t_time_score = t |> Schedule.get_tuesday |> check_classtime classtime in 
  let w_time_score = t |> Schedule.get_wednesday |> check_classtime classtime in 
  let th_time_score = t |> Schedule.get_thursday |> check_classtime classtime in 
  let f_time_score = t |> Schedule.get_friday |> check_classtime classtime in 
  m_time_score+.t_time_score+.w_time_score+.th_time_score+.f_time_score

let schedule_score (output:UserSurvey.t_output) (t:Schedule.t) = 
  let spread = if (output.spread_output = "N") then score_spread t else (1. -. score_spread t) in 
  let lunch = if (output.lunch_output = "N") then 1. else score_lunch t in 
  let classtime = score_classtime output.classtime t in 
  spread +. lunch +. classtime

(** [reverse_compare a b] is 1 when the score value of [a] is less than that of 
    [b], 0 when equal, -1 when greater. *)
let reverse_compare a b = 
  if (snd a) < (snd b) then 1
  else if (snd a) > (snd b) then -1
  else 0

(** [to_comp s] is the [comparable_event_bare] representation of
    schedule [s]. *)
let to_comp s =
  List.map (fun x-> to_bare_comparable_event x) s

(** [same_schedule_aux s1 s2] is [true] if [s2] is identical to schedule [s1].
    Else, [false]. *)
let rec same_schedule_aux s1 s2 =
  match s2 with
  | [] -> true
  | h::t -> begin
      if List.mem h s1 then same_schedule_aux s1 t else false
    end

(** [same_schedule r s] is [true] if [s] is identical to any schedule in [r].
    Else, [false]. *)
let rec same_schedule ranked sch =
  match ranked with
  | [] -> false
  | h::t -> begin
      let c_s1 = to_comp h and c_s2 = to_comp sch in
      if same_schedule_aux c_s1 c_s2 then true else same_schedule t sch
    end

(** [take_top_n n acc r_sch] takes the first [n] elements from the ranked schedule
    [r_sch]. If the length of [r_sch] is less that [n], then it returns [r_sch]. *)
let rec take_top_n n acc (r_sch:ranked_schedule) = 
  match r_sch with 
  | [] -> acc |> List.rev
  | hd::tl -> if List.length acc = n then acc |> List.rev
    else take_top_n n (hd::acc) tl

let rec rank_schedule n (acc:ranked_schedule) (output:UserSurvey.t_output) (t_list:t list) =
  match t_list with
  | [] -> List.sort reverse_compare acc |> take_top_n n []
  | h::t -> rank_schedule n ((h, schedule_score output h) :: acc) output t

let rec delete_dups acc_list sch_list =
  match sch_list with
  | [] -> acc_list
  | h::t -> if same_schedule (List.map (fun x-> Schedule.get_events x) acc_list) (Schedule.get_events h) then delete_dups acc_list t
    else delete_dups (h::acc_list) t