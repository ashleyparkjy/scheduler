open Yojson.Basic.Util

type course_id = int
type section_id = int
type meeting_id = int
type instructor_id = string

type time = {
  min: int;
  hr: int;
}
type day = 
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

exception BadUnits of course_id
exception BadQuery
exception ClassNotFound of course_id
exception SectionNotFound of section_id
exception MeetingNotFound of meeting_id
exception InstructorNotFound of instructor_id
exception BadDate of string
exception BadTime of string

(** The type of professor. *)
type professors = {
  first_name: string;
  middle_name: string;
  last_name: string;
  net_id: string;
}

(** The type of meeting. *)
type meetings = {
  class_mtg_number: meeting_id;
  time_start: time;
  time_end: time;
  instructors: professors list;
  pattern: day list;
  facility_descr: string;
  bldg_descr: string;
}

(** The type of section. *)
type sections = {
  section_type: string;
  section: string;
  class_nbr: section_id;
  meetings: meetings list;
  campus: string;
}

(** The type of class. *)
type classes = {
  crse_id: course_id;
  subject: string; 
  catalog_nbr: int;
  title_short: string;
  title_long: string;
  class_sections: sections list;
  units: int;
  components_required: string list;
  description: string;
}

type t = classes list

(** [to_time t] converts string in the format HH:MMXM to a time of type
    [time].
    Requires: t is a valid time string. *)
let to_time t =
  try
    let ampm = t.[5] and
    t_list = String.sub t 0 5 |> String.split_on_char ':' in
    let hour_raw = t_list |> List.hd |> int_of_string in
    let hour = match ampm with
      | 'A' -> if hour_raw = 12 then 0 else hour_raw
      | 'P' -> if hour_raw = 12 then 12 else 12+hour_raw
      | _ -> raise (BadTime t)
    in
    {
      min = List.nth t_list 1 |> int_of_string;
      hr = hour;
    }
  with Invalid_argument _ -> raise (BadTime t)

(** [to_time l t] converts [t] to a list of day types and appends it to
    [l].
    M -> Monday
    T -> Tuesday
    W -> Wednesday
    R -> Thursday
    F -> Friday
    Raises: [BadDate t] if date is invalid. *)
let rec to_day my_list d =
  if d = "" then my_list else
    let s = String.sub d 1 ((String.length d)-1) in
    match d.[0] with
    | 'M' -> to_day (Monday::my_list) s
    | 'T' -> to_day (Tuesday::my_list) s
    | 'W' -> to_day (Wednesday::my_list) s
    | 'R' -> to_day (Thursday::my_list) s
    | 'F' -> to_day (Friday::my_list) s
    | _ -> raise (BadDate d)

(** [to_string_robust] is the string of Yojson.Basic.t [x], but is "" if
    [x] is null. *)
let to_string_robust x =
  let y = x |> to_string_option in
  match y with
  | None -> ""
  | Some s -> s

(** [professor_of_json j] is a record of type for professorswith contents of j
    parsed into that type. *)
let professor_of_json j = {
  first_name = j |> member "firstName" |> to_string_robust;
  middle_name = j |> member "middleName" |> to_string_robust;
  last_name = j |> member "lastName" |> to_string_robust;
  net_id = j |> member "netid" |> to_string_robust;
}

(** [meeting_of_json j] is a record of type meetings with the contents of j
    parsed into that type. *)
let meeting_of_json j = {
  class_mtg_number = j |> member "classMtgNbr" |> to_int;
  time_start = j |> member "timeStart" |> to_string |> to_time;
  time_end = j |> member "timeEnd" |> to_string |> to_time;
  instructors = j |> member "instructors" |> to_list
                |> List.map professor_of_json;
  pattern = j |> member "pattern" |> to_string |> to_day [];
  facility_descr = j |> member "facilityDescr" |> to_string_robust;
  bldg_descr = j |> member "bldgDescr" |> to_string_robust;
}

(** [class_of_json j] is a record of type sections with the contents of j
    parsed into that type. *)
let section_of_json j = {
  section_type = j |> member "ssrComponent" |> to_string_robust;
  section = j |> member "section" |> to_string_robust;
  class_nbr = j |> member "classNbr" |> to_int;
  meetings = j |> member "meetings" |> to_list |> List.map meeting_of_json;
  campus = j |> member "campus" |> to_string_robust;
}

(** [class_of_json j] is a record of type classes with the contents of j
    parsed into that type. *)
let class_of_json j = 
  let x = match  j |> member "data" |> member "classes" |> to_list with
    | [] -> raise BadQuery
    | h::t -> h
  in
  let y = x |> member "enrollGroups" |> to_list |> List.hd
  in {
    crse_id = x |> member "crseId" |> to_int;
    subject = x |> member "subject" |> to_string_robust;
    catalog_nbr = x |> member "catalogNbr" |> to_string |> int_of_string;
    title_short = x |> member "titleShort" |> to_string_robust;
    title_long = x |> member "titleLong" |> to_string_robust;
    class_sections = y |> member "classSections" |> to_list
                     |> List.map section_of_json;
    units =
      begin
        let m = y |> member "unitsMinimum" |> to_int in
        if m = (y |> member "unitsMaximum" |> to_int) then m
        else -1
      end;
    components_required = y |> member "componentsRequired" |> to_list
                          |> List.map to_string_robust;
    description = x |> member "description" |> to_string_robust;
  }

let from_json json ros =
  (class_of_json json)::ros

let empty =
  []

let is_empty ros =
  match ros with
  | [] -> true
  | _ -> false

let rec size ros = 
  match ros with
  | [] -> 0
  | h::t -> 1 + size t

let rec course_ids ros =
  match ros with
  | [] -> []
  | h::t -> h.crse_id::(course_ids t)

(** [get_class c ros] is data of type classes that contains information on
    class [c] in [ros].
    Raises ClassNotFound if [c] is not valid. *)
let rec get_class c ros = 
  match ros with
  | [] -> raise (ClassNotFound c)
  | h::t -> if h.crse_id = c then h else get_class c t

let subject c ros =
  (get_class c ros).subject

let catalog_number c ros =
  (get_class c ros).catalog_nbr

let title_short c ros =
  (get_class c ros).title_short

let title_long c ros =
  (get_class c ros).title_long

let sections c ros =
  List.map (fun x-> x.class_nbr) (get_class c ros).class_sections

(** [get_section_helper s s_list] searches for and returns the section in
    s_list if that section has section_id of [s].
    Raises: [SectionNotFound] if [s] is not in [s_list]. *)
let rec get_section_helper s s_list =
  match s_list with
  | [] -> raise (SectionNotFound s)
  | h::t -> if h.class_nbr = s then h else get_section_helper s t

(** [get_section s c ros] is data of type sections that contains information on
    section [s] in class [c] in [ros].
    Raises: [ClassNotFound] if [c] is not a valid course id.
    [SectionNotFound] if [s] is not a valid section id. *)
let get_section s c ros = 
  let x = get_class c ros in
  get_section_helper s x.class_sections

let section_type s c ros =
  (get_section s c ros).section_type

let section_number s c ros =
  (get_section s c ros).section

let meetings s c ros =
  List.map (fun x-> x.class_mtg_number) (get_section s c ros).meetings

(** [get_meeting_helper m m_list] searches for and returns the meeting in
    [m_list] if that meeting has meeting_id of [m].
    Raises: [MeetingNotFound] if [m] is not in [m_list]. *)
let rec get_meeting_helper m m_list =
  match m_list with
  | [] -> raise (MeetingNotFound m)
  | h::t -> if h.class_mtg_number = m then h else get_meeting_helper m t

(** [get_meeting m s c ros] is data of type meetings that contains information
    on meeting [m] in section [s] in class [c] in [ros].
    Raises: [ClassNotFound] if [c] is not a valid course id.
    [SectionNotFound] if [s] is not a valid section id.
    [MeetingNotFound] if [m] is not a valid meeting id. *)
let get_meeting m s c ros = 
  let x = get_section s c ros in
  get_meeting_helper m x.meetings

let start_time m s c ros =
  (get_meeting m s c ros).time_start

let end_time m s c ros =
  (get_meeting m s c ros).time_end

let instructors m s c ros =
  List.map (fun x-> x.net_id) (get_meeting m s c ros).instructors

(** [get_instructor_helper i i_list] searches for and returns the instructor
    in [i_list]if that instructor has net_id [i].
    Raises: [InstructorNotFound] if [i] is not in [i_list]. *)
let rec get_instructor_helper i i_list =
  match i_list with
  | [] -> raise (InstructorNotFound i)
  | h::t -> if h.net_id = i then h else get_instructor_helper i t

(** [get_instructor i m s c ros] is data of type instructor that contains
    information on instructor [i] in meeting [m] in section [s] in class [c]
    in [ros].
    Raises: [ClassNotFound] if [c] is not a valid course id.
    [SectionNotFound] if [s] is not a valid section id.
    [MeetingNotFound] if [m] is not a valid meeting id.
    [InstructorNotFound] if [i] is not a valid instructor id. *)
let get_instructor i m s c ros = 
  let x = get_meeting m s c ros in
  get_instructor_helper i x.instructors

let instructor_name i m s c ros =
  let prof = (get_instructor i m s c ros) in
  if prof.middle_name = "" then prof.first_name ^ " " ^ prof.last_name
  else prof.first_name ^ " " ^ prof.middle_name ^ " " ^ prof.last_name

let pattern m s c ros =
  (get_meeting m s c ros).pattern

let facility_description m s c ros =
  (get_meeting m s c ros).facility_descr

let building_description m s c ros =
  (get_meeting m s c ros).bldg_descr

let campus s c ros =
  (get_section s c ros).campus

let units c ros =
  let x = (get_class c ros).units in
  if x >= 0 then x else raise (BadUnits c) 

let components_required c ros =
  (get_class c ros).components_required

let description c ros = 
  (get_class c ros).description

let time_to_int t =
  t.hr*60+t.min