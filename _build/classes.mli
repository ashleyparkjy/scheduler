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

(** The type of instructor identifier. *)
type instructor_id = string

(** The type of time in 24-hour format. *)
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

(** Raised when a JSON search query does not return any results. *)
exception BadQuery

(** Raised when the class is not found. *)
exception ClassNotFound of course_id

(** Raised when the section is not found. *)
exception SectionNotFound of section_id

(** Raised when the meeting is not found. *)
exception MeetingNotFound of meeting_id

(** Raised when the instructor is not found. *)
exception InstructorNotFound of instructor_id

(** Raised when date is of bad type. *)
exception BadDate of string

(** Raised when time is of bad type. *)
exception BadTime of string

(** [from_json j t] is the class compilation data type containing [t] and the
    additional class that [j] represents.
    Requires: [j] is a valid JSON class representation with only one enroll
    group and [t] is a valid type [t].
    Raises: [BadQuery] if JSON contains no class data. *)
val from_json : Yojson.Basic.t -> t -> t

(** [empty] is a data type of type t representing no classes. *)
val empty: t

(** [is_empty j] is [true] if [j] represents no classes. Else, [false]. *)
val is_empty: t -> bool

(** [size t] is the size of the class list denoted by [t]. *)
val size: t -> int

(** [course_ids c] is a list of all course id's in [c]. *)
val course_ids: t -> course_id list

(** [subject t c] is the subject of course [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val subject: course_id -> t -> string

(** [catalog_number t c] is the catalog number of course [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val catalog_number: course_id -> t -> int

(** [title_short t c] is the shortened title of course [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val title_short: course_id -> t -> string

(** [title_short t c] is the full title of course [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val title_long: course_id -> t -> string

(** [sections t c] is the list of section numbers of course [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val sections: course_id -> t -> section_id list

(** [section_type s c t] is the section type of section [s] of course [c] in
    [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id. *)
val section_type: section_id -> course_id -> t -> string

(** [section_number s c t] is the section number of section [s] of course [c] in
    [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id. *)
val section_number: section_id -> course_id -> t -> string

(** [meetings s c t] is the list of meeting id's for section [s] of course [c]
    in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id. *)
val meetings: section_id -> course_id -> t -> meeting_id list

(** [start_time m s c t] is the start time of meeting [m] of section [s] in
    class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id. *)
val start_time: meeting_id -> section_id -> course_id -> t -> time

(** [end_time m s c t] is the end time of meeting [m] of section [s] in
    class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id. *)
val end_time: meeting_id -> section_id -> course_id -> t -> time

(** [instructors m s c t] is a list of instructor id's of meeting [m] of
    section [s] in class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id. *)
val instructors: meeting_id -> section_id -> course_id -> t -> instructor_id list

(** [instructor_name i m s c t] is the full name of instructor [i] of
    meeting [m] of section [s] in class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id.
    [InstructorNotFound i] if [i] is not a valid instructor id. *)
val instructor_name: instructor_id -> meeting_id -> section_id -> course_id -> t -> string

(** [pattern m s c t] is the meeting pattern of meeting [m] of section [s] in
    class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id. *)
val pattern: meeting_id -> section_id -> course_id -> t -> day list

(** [facility_description m s c t] is the description of the meeting facility
    of meeting [m] of section [s] in class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id. *)
val facility_description: meeting_id -> section_id -> course_id -> t -> string

(** [building_description m s c t] is the description of the building of
    meeting [m] of section [s] in class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id.
    [MeetingNotFound m] if [m] is not a valid meeting id. *)
val building_description: meeting_id -> section_id -> course_id -> t -> string

(** [campus s c t] is the campus location of section [s] of course [c] in
    [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [SectionNotFound s] if [s] is not a valid course id. *)
val campus: section_id -> course_id -> t -> string

(** [units c t] is the number of units correlated with the class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id.
    [BadUnits c] if units are not consistent. *)
val units: course_id -> t -> int

(** [components_required c t] is a list of the components required to take
    class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val components_required: course_id -> t -> string list

(** [description c t] is the description of class [c] in [t].
    Raises: [ClassNotFound c] if [c] is not a valid course id. *)
val description: course_id -> t -> string