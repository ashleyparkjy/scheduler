(** Produces a JSON file with relevant course data from the Cornell class
    roster API, using user-inputted specifications. 
    Code base courtesy of https://github.com/avsm/ocaml-cohttp.*)

(** Raised when a bad url is encountered. *)
exception BadUrl of int

(** [make_url s u c] is a URL containing a JSON file for class [c] in
    subject [u] in semester [s]. *)
val make_url : string -> string -> string -> string

(** [get_json url] is a data structure of type [string Lwt.t] containing the
    course data using GET on [url]. Raises a failure if the url is invalid. *)
val get_json : string -> string Lwt.t

(** [runner s u c] saves a JSON file to the directory containing this file with
    the nomencalture [s] ^ [u] ^ [c] ^ ".json". The file contains data on class
    [c] in subject [u] in semester [s].
    Example: [runner "SP20" "CS" "3110"] produces a file called
    "SP20_CS_3110.json" with course data regarding CS 3110 from SP20.*)
val runner : string -> string -> string -> unit
