
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

(** [dimension_builder s d] builds a type [dimensions] with size [s] and
    density [d]. *)
let dimension_builder s d =
  let (w,h) = s in
  let h = begin if d = 1 then 2 else if d = 2 then 4 else if d = 3 then 6 else failwith "bad density" end in
  {
    time = w/11;
    day = 2*w/11;
    hour = h;
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

(** [build_header density d] builds the header of the schedule using data from
    type dimensions [d]. *)
let build_header density d =
  let (w,h) = d and
  d_log = dimension_builder d density in
  printer "\n";
  repeater "-" w;
  printer "\n";
  repeater " " ((d_log.time-4)/2);
  printer "Time";
  repeater " " ((d_log.time-4)/2-1);
  printer "|";
  List.fold_left (fun init x-> build_header_day d_log x) ()
    ["Monday";"Tuesday";"Wednesday";"Thursday";"Friday"];
  printer "\n";
  repeater "-" w

(** [cur_time h d l] returns the time range in schedule in Class.time format. *)
let cur_time hr d_log line=
  let hour_lines = d_log.hour in
  let start_min = (line-1)*60/hour_lines and
    end_min = line*60/hour_lines in
  let start =
    Classes.{
      min = start_min;
      hr = hr;
    } and
    endt = begin
      if end_min = 60 then
        Classes.{
          min = 0;
          hr = hr+1;
        } 
      else
        Classes.{
          min = end_min;
          hr = hr;
        }
    end
  in
  (start, endt)

(** [in_class log d t evs] is the option event of the class that is occuring
    at time [t] in day [d]. Else, [None]. *)
let rec in_class d_log day tr (evs : Schedule.event list) =
  match evs with
  | [] -> None
  | h::t -> if (List.mem day h.days) then begin
      let min_mins = 60/d_log.hour/2 in
      let (s,e) = tr in
      let h_start = Classes.time_to_int h.start_time and
        h_end = Classes.time_to_int h.end_time and
        tr_start = Classes.time_to_int s and
        tr_end = Classes.time_to_int e in
      if h_start <= tr_start && h_end >= tr_end then Some h
      else if h_start > tr_start && h_start < tr_end && h_end < tr_end && h_end > tr_start then Some h
      else if h_end < tr_start then in_class d_log day tr t
      else if h_start > tr_end then in_class d_log day tr t
      else if h_start <= tr_start && h_end < tr_end && h_end >= tr_start then begin if h_end-tr_start >= min_mins then Some h else in_class d_log day tr t end
      else if h_start > tr_start && h_start <= tr_end && h_end >= tr_end then begin if tr_end-h_start >= min_mins then Some h else in_class d_log day tr t end
      else in_class d_log day tr t
    end
    else in_class d_log day tr t

(** [hourly_aux d hr evs] fills in class data into terminal schedule for
    any time [t] from events [evs]. *)
let rec hourly_aux2 d_log (time : Classes.time * Classes.time) evs (week : Classes.day list) =
  match week with
  | [] -> ()
  | h::t ->
    begin
      match in_class d_log h time evs with
      | None -> repeater " " (d_log.day-1)
      | Some ev -> begin
          let this_class = ev.subject ^ " " ^ (string_of_int ev.catalog_number) ^ " " ^ ev.event_type ^ " " ^ ev.section_number in
          let class_len = String.length this_class and
            space = d_log.day-1 in
          if class_len > space then printer (String.sub this_class 0 space)
          else printer this_class; repeater " " (space-class_len)
        end;
    end;
    printer  "|";
    hourly_aux2 d_log time evs t

(**[format_time n] formats int [n] to a 2 place string *)
let format_time n =
  if n>=10 then string_of_int n else "0" ^ (string_of_int n)

(** [hourly_aux a d sch h] fills in class data into terminal schedule for
    non-1st line of each hour. *)
let rec hourly_aux acc d_log sch hr =
  match acc with
  | 0 -> ()
  | _ -> 
    let time_range = cur_time hr d_log (1+d_log.hour-acc) in
    let (st,en) = time_range in
    repeater " " ((d_log.time-4)/2-1);
    printer ((format_time st.hr) ^ ":" ^ format_time (st.min));
    repeater " " ((d_log.time-4)/2-1);
    printer "|";
    hourly_aux2 d_log time_range sch [Classes.Monday; Classes.Tuesday; Classes.Wednesday; Classes.Thursday; Classes.Friday];
    printer "\n";
    hourly_aux (acc-1) d_log sch hr

(** [hourly_printer density c_size sch hr] rints an hour of schedule that is
    the hour [hr], and schedule [sch], formatted to [c_size]. *)
let hourly_printer density c_size sch hr =
  let (w,h) = c_size and
  d_log = dimension_builder c_size density in
  printer "\n";
  let hourly_events = Schedule.get_hour [] hr sch in
  hourly_aux d_log.hour d_log hourly_events hr;
  repeater "-" w


(** [build_schedule density c sch] prints the schedule given by schedule [sch]
    by size [c]. *)
let build_schedule density c_size sch =
  List.map (fun x-> hourly_printer density c_size sch x)
    [8;9;10;11;12;13;14;15;16;17;18;19;20;21] |> ignore;
  ()

(** [print_schedule sch] prints the schedule [sch] in table format. *)
let print_schedule density sch =
  let c_size = chart_size terminal_size in
  build_header density c_size;
  build_schedule density c_size sch

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

(** [visualize_schedule rank density score sch] visualizes the schedule [sch]
    as the [rank] rank with denisty [density]. *)
let visualize_schedule rank density score sch =
  printer ("\n\nSchedule #" ^ (string_of_int rank) ^ "\n");
  printer ("Score: " ^ (string_of_int score));
  print_schedule density sch;
  printer "\n";
  let data_bank = pull_event_data [] sch in
  List.map (fun x-> class_printer x) data_bank |> ignore

(** [user_preference]] returns the numerical preference for schedule density. *)
let rec user_pref () =
  printer "\nInput your schedule density preference (1=compact, 2=default, 3=comfortable):\n>";
  let pref = read_line () in
  if pref = "1" then 1
  else if pref = "2" then 2
  else if pref = "3" then 3
  else user_pref ()

let visualize r =
  let density = user_pref () in
  let (w,h) = terminal_size in 
  ANSITerminal.resize w h;

  List.fold_left (fun init x-> let (sch,score) = x in visualize_schedule init density score (Schedule.get_events sch); init+1) 1 r |> ignore;
  ()
(*
  let (sch,score) = List.hd r in
  visualize_schedule 1 density score (Schedule.get_events sch)
*)