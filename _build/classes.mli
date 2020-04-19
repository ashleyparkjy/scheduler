(** 
   Representation of a list of course data.

   This module represents the data stored in course json files from the Class
   Roster API.
*)

(** The abstract type of values representing a compilation of courses. *)
type t

(** The type of course identifier. *)
type course_id = int

(** The type of section identifier. *)
type section_id = int

(** The type of class meeting identifier. *)
type meeting_id = int

(** The type of time. *)
type time = {
  min: int;
  hr: int;
}

(** The type of day. *)
type day =
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

(** Raised when an inconsistent amount of units is encountered for a course. *)
exception BadUnits of course_id

(** Raised when the class is not found. *)
exception ClassNotFound

(** Raised when date is of bad type. *)
exception BadDate of string

(** Raised when time is of bad type. *)
exception BadTime of string

(** [from_json t j] is the class compilation data type containing [t] and the
    additional class that [j] represents.
    Requires: [j] is a valid JSON class representation with only one enroll
    group and [t] is a valid type [t]. *)
val from_json : t -> Yojson.Basic.t -> t

(** [empty] is a data type of type t representing no classes. *)
val empty: t

(** [is_empty j] is [true] if [j] represents no classes. Else, [false]. *)
val is_empty: t -> bool

(** [size t] is the size of the class list denoted by [t]. *)
val size: t -> int

(** [course_ids c] is a list of all course id's in [c]. *)
val course_ids: t -> course_id list

(** [subject t c] is the subject of course [c] in [t]. *)
val subject: t -> course_id -> string

(** [catalog_number t c] is the catalog number of course [c] in [t]. *)
val catalog_number: t -> course_id -> int

(** [title_short t c] is the shortened title of course [c] in [t]. *)
val title_short: t -> course_id -> string

(** [title_short t c] is the full title of course [c] in [t]. *)
val title_long: t -> course_id -> string

(** [sections t c] is the list of section numbers of course [c] in [t]. *)
val sections: t -> course_id -> section_id list

(** [section_type t c s] is the section type of section [s] of course [c] in
    [t]. *)
val section_type: t -> course_id -> section_id -> string

(** [class_number t c s] is the class number of section [s] of course [c] in
    [t]. *)
val class_number: t -> course_id -> section_id -> int

(** [meetings t c s] is the list of meeting id's for section [s] of course [c]
    in [t]. *)
val meetings: t -> course_id -> section_id -> meeting_id

(** [start_time t c s m] is the start time of meeting [m] of section [s] in
    class [c] in [t]. *)
val start_time: t -> course_id -> section_id -> meeting_id -> time

(** [end_time t c s m] is the end time of meeting [m] of section [s] in
    class [c] in [t]. *)
val end_time: t -> course_id -> section_id -> meeting_id -> time

(** [instructors t c s m] is a list of instructors of meeting [m] of
    section [s] in class [c] in [t]. *)
val instructors: t -> course_id -> section_id -> meeting_id -> string list

(** [pattern t c s m] is the meeting pattern of meeting [m] of section [s] in
    class [c] in [t]. *)
val pattern: t -> course_id -> section_id -> meeting_id -> day list

(** [facility_description t c s m] is the description of the meeting facility
    of meeting [m] of section [s] in class [c] in [t]. *)
val facility_description: t -> course_id -> section_id -> meeting_id -> string

(** [building_description t c s m] is the description of the building of
    meeting [m] of section [s] in class [c] in [t]. *)
val building_description: t -> course_id -> section_id -> meeting_id -> string

(** [campus t c s] is the campus location of section [s] of course [c] in
    [t]. *)
val campus: t -> course_id -> section_id -> string

(** [units t c] is the number of units correlated with the class [c] in [t].
    If number of units is inconsistent, raise [BadUnits c]. *)
val units: t -> course_id -> int

(** [components_required t c] is a list of the components required to take
    class [c] in [t]. *)
val components_required: t -> course_id -> string list