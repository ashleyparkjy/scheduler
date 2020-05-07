
type t = (Schedule.t * int) list

(** [dimensions] holds the dimensions for the table with respective
    measurements of the time, day, hour, and header. *)
type dimensions = {
  time: int;
  day: int;
  hour: int;
}

(** [section] is the type of section. *)
type section = {
  section_id: int;
  section_number: string;
  section_type: string;
  instructor_names: string list;
}

(** [info_library] is a record containing pertinant information on a single
    class configuration. *)
type info_library = {
  class_id: string;
  class_name: string;
  location: string;
  credits: int;
  sections: section list;
}

(**[style] is the style of ANSITerminal outputs. *)
let my_style =
  ANSITerminal.cyan

(**[printer s] prints s. *)
let printer s =
  ANSITerminal.print_string [my_style] s

(** [terminal_size] is the assumed character width and height of the terminal
    viewing the visualization. *)
let terminal_size =
  (120,40)

(** [chart_size s] culls down terminal size [s] into a size that fits nicely
    onto the existing terminal window. *)
let chart_size s =
  let (w,h) = s in
  (w*9/10,h*9/10)

(** [dimension_builder s] builds a type [dimensions] with size [s]. *)
let dimension_builder s =
  let (w,h) = s in
  {
    time = w/11;
    day = 2*w/11;
    hour = 4;
  }

(** [repeater c l] prints character [c] to ANSITerminal [l] times. *)
let rec repeater c l =
  match l with
  | 0 -> ()
  | _ ->  printer c; repeater c (l-1)

(**[build_header_day] is a helper function for [build_header], formatting the
   days in the schedule header. *)
let build_header_day d_log day =
  repeater " " ((d_log.day-(String.length day))/2);
  printer day;
  repeater " " ((d_log.day-(String.length day)-1)/2);
  printer  "|"

(** [build_header d] builds the header of the schedule using data from type
    dimensions [d]. *)
let build_header d =
  let (w,h) = d and
  d_log = dimension_builder d in
  printer "\n";
  repeater "-" w;
  printer "\n";
  repeater " " ((d_log.time-4)/2);
  printer "Time";
  repeater " " ((d_log.time-5)/2);
  printer "|";
  List.fold_left (fun init x-> build_header_day d_log x) () ["Monday";"Tuesday";"Wednesday";"Thursday";"Friday"];
  printer "\n";
  repeater "-" w

(** [build_schedule sch] prints the schedule given by schedule [sch]. *)
let build_schedule sch =
  ()

(** [print_schedule sch] prints the schedule [sch] in table format. *)
let print_schedule sch =
  let c_size = chart_size terminal_size in
  build_header c_size;
  build_schedule sch

(** [pull_section_data e] is a record list of type [section], containing all
    section data from [e]. *)
let rec pull_section_data (ev: Schedule.event) =
  {
    section_id = ev.section_id;
    section_number = ev.section_number;
    section_type = ev.event_type;
    instructor_names = ev.instructors;
  }

(** [pull_event_data acc_list sch] pulls relevant event data from [sch] *)
let rec pull_event_data acc_list (sch : Schedule.event list) =
  match sch with
  | [] -> acc_list
  | h::t ->
    let c_name = h.subject ^ " " ^ (string_of_int h.catalog_number) in
    if List.mem c_name (List.map (fun x-> x.class_id) acc_list) then
      let new_list =
        List.map (fun x->
            if x.class_id = c_name then
              {
                class_id = x.class_id;
                class_name = x.class_name;
                location = x.location;
                credits = x.credits;
                sections = (pull_section_data h)::x.sections;
              }
            else x
          ) acc_list in
      pull_event_data new_list t
    else
      let new_class =  {
        class_id = c_name;
        class_name = h.course_name;
        location = h.facility;
        credits = h.credits;
        sections = [pull_section_data h];
      } in
      pull_event_data (new_class::acc_list) t

(** [section_printer s] prints list of section data from [s] to the terminal. *)
let section_printer s =
  printer ((string_of_int s.section_id) ^ (" (" ^ s.section_type ^ " " ^ s.section_number ^ "), "))

(** [class_printer c] prints list of class data from [c] to the terminal. *)
let class_printer c =
  printer (c.class_id ^ " (" ^ c.class_name ^ ")\n");
  printer "Sections: ";
  List.map (fun x-> section_printer x) c.sections |> ignore;
  printer "\n";
  printer ("Location: " ^ c.location ^ "\n");
  printer "Credits: ";
  if c.credits = -1 then printer "Inconsistent" else printer (string_of_int c.credits);
  printer "\n"

(** [visualize_schedule rank sch] visualizes the schedule [sch] as the
    [rank] rank. *)
let visualize_schedule rank score sch =
  printer ("\n\nSchedule #" ^ (string_of_int rank) ^ "\n");
  printer ("Score: " ^ (string_of_int score));
  print_schedule sch;
  printer "\n";
  let data_bank = pull_event_data [] sch in
  List.map (fun x-> class_printer x) data_bank |> ignore

let visualize r =
  let (w,h) = terminal_size in 
  ANSITerminal.resize w h;
(*
  List.fold_left (fun init x-> let (sch,score) = x in visualize_schedule init score (Schedule.get_events sch); init+1) 1 r |> ignore;
  ()
*)
  let (sch,score) = List.hd r in
  visualize_schedule 1 score (Schedule.get_events sch)