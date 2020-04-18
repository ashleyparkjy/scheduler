open Yojson.Basic.Util

type course_id = int
type section_id = int
type meeting_id = int
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
exception ClassNotFound
exception BadDate of string
exception BadTime of string

(** The type of meeting. *)
type meetings ={
  class_mtg_number: meeting_id;
  time_start: time;
  time_end: time;
  instructors: string list;
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
  components_required: string list
}

type t = classes list

(** [to_time t] converts string in the format HH:MMXM to a time of type
    [time].
    Requires: t is a valid time string. *)
let to_time t =
  let ampm = t.[5] and
  t_list = String.sub t 0 5 |> String.split_on_char ':' in
  let hour_raw = t_list |> List.hd |> int_of_string in
  let hour = match ampm with
    | 'A' -> if hour_raw = 12 then 0 else hour_raw
    | 'P' -> if hour_raw = 12 then 12 else hour_raw mod 12
    | _ -> raise (BadTime t)
  in
  {
    min = List.nth t_list 1 |> int_of_string;
    hr = hour;
  }

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

(** [meeting_of_json j] is a record of type meetings with the contents of j
    parsed into that type. *)
let meeting_of_json j = {
  class_mtg_number = j |> member "classMtgNbr" |> to_int;
  time_start = j |> member "timeStart" |> to_string |> to_time;
  time_end = j |> member "timeEnd" |> to_string |> to_time;
  instructors = j |> member "instructors" |> to_list |> List.map to_string;
  pattern = j |> member "pattern" |> to_string |> to_day [];
  facility_descr = j |> member "facilityDescr" |> to_string;
  bldg_descr = j |> member "bldgDescr" |> to_string;
}

(** [class_of_json j] is a record of type sections with the contents of j
    parsed into that type. *)
let section_of_json j = {
  section_type = j |> member "ssrComponent" |> to_string;
  section = j |> member "section" |> to_string;
  class_nbr = j |> member "classNbr" |> to_int;
  meetings = j |> member "meetings" |> to_list |> List.map meeting_of_json;
  campus = j |> member "campus" |> to_string;
}

(** [class_of_json j] is a record of type classes with the contents of j
    parsed into that type. *)
let class_of_json j = 
  let x = match  j |> member "data" |> member "classes" |> to_list with
    | [] -> raise ClassNotFound
    | h::t -> h
  in {
    crse_id = x |> member "crseID" |> to_int;
    subject = x |> member "subject" |> to_string;
    catalog_nbr = x |> member "catalogNbr" |> to_int;
    title_short = x |> member "titleShort" |> to_string;
    title_long = x |> member "titleLong" |> to_string;
    class_sections = x |> member "enrollGroups" |> to_list |> List.hd |> member "classSections" |> to_list |> List.map section_of_json;
    units =
      begin
        let m = x |> member "unitsMinimum" |> to_int in
        if m = (x |> member "unitsMaximum" |> to_int) then m else raise (BadUnits (x |> member "crseID" |> to_int))
      end;
    components_required = x |> member "componentsRequired" |> to_list |> List.map to_string;
  }

let from_json ros json =
  (class_of_json json)::ros

let empty =
  failwith "Unimplemented"

let is_empty ros =
  failwith "Unimplemented"

let course_ids ros =
  failwith "Unimplemented"

let subject ros c =
  failwith "Unimplemented"

let catalog_number ros c =
  failwith "Unimplemented"

let title_short ros c =
  failwith "Unimplemented"

let title_long ros c =
  failwith "Unimplemented"

let sections ros c =
  failwith "Unimplemented"

let section_type ros c s =
  failwith "Unimplemented"

let class_number ros c s =
  failwith "Unimplemented"

let meetings ros c s =
  failwith "Unimplemented"

let start_time ros c s m =
  failwith "Unimplemented"

let end_time ros c s m =
  failwith "Unimplemented"

let instructors ros c s m =
  failwith "Unimplemented"

let pattern ros c s m =
  failwith "Unimplemented"

let facility_description ros c s m =
  failwith "Unimplemented"

let building_description ros c s m =
  failwith "Unimplemented"

let campus ros c s =
  failwith "Unimplemented"

let units ros c =
  failwith "Unimplemented"

let components_required ros c =
  failwith "Unimplemented"