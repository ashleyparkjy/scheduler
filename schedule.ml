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

(** [make_event m s c r] makes an event data type of meeting [m] in section [s]
    in class [c] in roster [r]. *)
let make_event m s c r =
  {
    course_name = Classes.title_long c r;
    description = Classes.description c r;
    course_id = c;
    credits = Classes.units c r;
    subject = Classes.subject c r;
    catalog_number = Classes.catalog_number c r;
    event_type = Classes.section_type s c r;
    section_id = s;
    section_number = Classes.section_number s c r;
    meeting_id = m;
    start_time = Classes.start_time m s c r;
    end_time = Classes.end_time m s c r;
    instructors = List.map (fun x-> Classes.instructor_name x m s c r) (Classes.instructors m s c r);
    facility = Classes.facility_description m s c r;
    building = Classes.building_description m s c r;
  }

let add_section s c r t =
  t@(List.map (fun x-> make_event x s c r) (Classes.meetings s c r))

let empty =
  []

let is_empty t =
  match t with
  | [] -> true
  | _ -> false

let size t =
  List.length t

let peak t =
  List.hd t

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