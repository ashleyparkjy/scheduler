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
  days: Classes.day list;
}

type t = event list

type comparable_t = {
  event_object: t;
  start_time_min: int;
  end_time_min: int;
}

type t_valid = {
  events: t;
  start_time_valid: bool;
}


let time_to_minute =
  failwith "unimplemented"

let sort_and_check_start_time = 
  failwith "unimplemented"

(*    let rec overlap t acc = 
      match t.classes with
      | [] -> 
      { classes: acc
      valid: false}
      | _ -> if check is not true then {classes: []; valid: false} else overlap {classes: []; valid=true} hd::acc *)

(*    let rec overlap t(from scheduler maker element) day_classes acc = 
      match day_classes with
      | [] -> 
      { schedule: t
      valid: false}
      | _ -> if check is not true then {classes: []; valid: false} else overlap {classes: []; valid=true} hd::acc *)

let check_end_time = 

  (*    check previous start time with -> 
        1) class1_end < class2_start = 
        class2_start <= class1_end <= class2_end && class1_start = class2_start <= class2_end <= class1_end *)

  let check_days = 
    failwith "unimplemented"
(* sort_and_check_start_time & check_end_time for each day *)
(* if true output event list <-> if false output [] *)

(** 
   get mon, tue, wed, thurs, fri -> event list
   get start/end time for each event in minutes
   [(start time, end time), ..]
   order list based on start time -> what if same time? -> if same time return false

   dat type {
    ...
    valid: false/true
    ...
   }

   for each t list
    each event list s (schedule)

   rec func2 = ordered_list 
   func s

   match acc
    | [] -> {acc}
    | < -> false 
    { classes: []
      valid:false; }, true acc append

   valid = true 
   check start time with -> 
   1) class1_start < class2_start 
   && 2) class1_end < class2_start

   check previous start time with -> 
   1) class1_end < class2_start
   class2_start <= class1_end <= class2_end
   class1_start = class2_start <= class2_end <= class1_end
   class 2 compare with 3

*)


