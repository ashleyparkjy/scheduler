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

type t = event list

let add_section s c r t =
  failwith "Unimplemented"

let empty =
  []

let is_empty t =
  match t with
  | [] -> true
  | _ -> false

let size t =
  failwith "Unimplemented"

let get_events t =
  failwith "Unimplemented"

let get_monday t =
  failwith "Unimplemented"

let get_tuesday t =
  failwith "Unimplemented"

let get_wednesday t =
  failwith "Unimplemented"

let get_thursday t =
  failwith "Unimplemented"

let get_friday t =
  failwith "Unimplemented"