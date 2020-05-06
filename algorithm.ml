

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
  | [] -> failwith "empty list"
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





(** TODO - calculates the total time of classes for the specified day [event_l] *)
let rec day_class_time acc event_l =
  match event_l with
  | hd::tl -> day_class_time ((hd |> to_comparable_event).end_time_min - (hd |> to_comparable_event).start_time_min + acc) tl
  | [] -> acc

(** TODO - calulates spread score of a schedule with standard deviation *)
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


(** TODO - checks if the day schedule has a lunch time of at least 60 minutes in
    between 11AM and 1PM. true if it does. else false *)
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

(** TODO - calulates lunch score of a schedule. 
    If Y - score_lunch of a schedule is sum of score of each day (0.2 if has lunch
    time between 11:00 and 1:00 and 0 if doesn't. *)
let rec score_lunch t =
  let m_lunch_score = if t|>Schedule.get_monday |> check_lunch_time  then 0.2 else 0. in
  let t_lunch_score = if t|>Schedule.get_tuesday |> check_lunch_time  then 0.2 else 0. in
  let w_lunch_score = if t|>Schedule.get_wednesday |> check_lunch_time  then 0.2 else 0. in
  let th_lunch_score = if t|>Schedule.get_thursday |> check_lunch_time  then 0.2 else 0. in
  let f_lunch_score = if t|>Schedule.get_friday |> check_lunch_time  then 0.2 else 0. in
  m_lunch_score+.t_lunch_score+.w_lunch_score+.th_lunch_score+.f_lunch_score

(** TODO *)
let score_classtime t = failwith "unimplemented"

(** TODO *)
let schedule_score st t = 
  failwith "score_spread + (if st.lunch_output = 'N' then 1 else score_lunch t) + score_classtime"

(** TODO *)
let rank_schedule st t_list =
  failwith "unimplemented"




