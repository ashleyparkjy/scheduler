open Yojson.Basic.Util

type course_id = string
type section_id = int
type meeting_id = int
type time = int
type day = string
exception BadUnits of course_id


type t

let from_json ros json =
  failwith "Unimplemented"

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