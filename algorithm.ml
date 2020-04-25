

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




