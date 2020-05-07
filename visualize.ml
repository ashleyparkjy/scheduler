
type t = Schedule.t list

(** [dimensions] holds the dimensions for the table with respective
    measurements of the time, day, hour, and header. *)
type dimensions = {
  time: int;
  day: int;
  hour: int;
  header: int;
}

(**[style] is the style of ANSITerminal outputs. *)
let my_style =
  ANSITerminal.cyan

(** [terminal_size] is the assumed character width and height of the terminal
    viewing the visualization. *)
let terminal_size =
  (120,30)

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
    hour = h/16;
    header = h/8;
  }

(** [repeater c l] prints character [c] to ANSITerminal [l] times. *)
let rec repeater c l =
  match l with
  | 0 -> ()
  | _ ->  ANSITerminal.print_string [my_style] c; repeater c (l-1)

(** [build_header d] builds the header of the schedule using data from type
    dimensions [d]. *)
let build_header d =
  let (w,h) = d in
  ANSITerminal.print_string [my_style] "\n";
  repeater "-" w

let visualize r =
  (*ANSITerminal.(print_string [default] (let (a,b) = size () in string_of_int a))*)
  let (w,h) = terminal_size in 
  ANSITerminal.resize w h;
  ANSITerminal.(print_string [my_style] "\n\nHere is the visualization :)\n");
  let c_size = chart_size terminal_size in
  build_header c_size;
  ANSITerminal.print_string [my_style] "\n\n"