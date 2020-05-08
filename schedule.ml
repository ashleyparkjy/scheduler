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
  days: Classes.day list;
}

type permutation = {
  course_id : Classes.course_id;
  permutations : (Classes.section_id list) list;
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
    days = Classes.pattern m s c r;
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
  t

(** [get_day d a c] is a list of events from [c] that occurs on day [d],
    appended to [a]. *)
let rec get_day day acc_list c =
  match c with
  | [] -> acc_list
  | h::t ->
    begin
      if (List.mem day h.days) then get_day day (h::acc_list) t
      else get_day day acc_list t
    end

let get_monday t =
  get_day Classes.Monday [] t

let get_tuesday t =
  get_day Classes.Tuesday [] t

let get_wednesday t =
  get_day Classes.Wednesday [] t

let get_thursday t =
  get_day Classes.Thursday [] t

let get_friday t =
  get_day Classes.Friday [] t

(** [is_occurring n ev] is [true] if [ev] is taking place during hour [n]. *)
let is_occurring n ev =
  if n >= ev.start_time.hr && n <= ev.end_time.hr then true else false

let rec get_hour acc_list n tea =
  match tea with
  | [] -> acc_list
  | h::t -> if (is_occurring n h) then get_hour (h::acc_list) n t else get_hour acc_list n t

(** [perm_to_sched l p] is a list of schedule configurations where each config
    is a list of sections for each class, section permutations in a tuple of
    course id and type [permutation] from [p]. Appended to [l].
    Example: [[(593554,[10601;19118]);(593554,[10601;19118])];[(593554,[10601;19118]);(593554,[10601;19118])]] *)
let rec perm_to_sched acc_list p =
  match p with
  | [] -> [acc_list]
  | h::t -> List.fold_left (fun init x-> init@(perm_to_sched ((h.course_id,x)::acc_list) t)) [] h.permutations

(** [parse_sections l req s c r] is the list of section id's from [s] from
    course [c] in roster [r] that satisfy being of section type [req].
    Appended to list [l]. *)
let rec parse_sections acc_list req sections course ros =
  match sections with
  | [] -> acc_list
  | h::t ->
    begin
      if Classes.section_type h course ros = req then parse_sections (h::acc_list) req t course ros
      else parse_sections acc_list req t course ros
    end

(** [list_of_list a req s c r] is a list of lists, where each list of list is a
    list of section id's that require one part of [req], where the sections are
    drawn from [s_list] from course [c] in roster [r]. Results appended to [a].
    Example: [[10601;10602];[10603,10604]] *)
let rec list_of_list acc_list comp_req s_list course ros =
  match comp_req with
  | [] -> acc_list
  | h::t -> list_of_list ((parse_sections [] h s_list course ros)::acc_list) t s_list course ros

(** [generate_perms a p] is a list of list of section id's, representing all
    possible permuations of sections from list of list of section id's, where
    each list within a list is all the section id's for one required
    component. Appended to [a]. *)
let rec generate_perms acc_list p_list =
  match p_list with
  | [] -> [acc_list] 
  | h::t -> List.fold_left (fun init x-> init@(generate_perms (x::acc_list) t)) [] h

(** [class_permutations c r] is a record of type [permutation] containing all
    section permutations of class [c] in roster [r]. *)
let class_permutations cls ros =
  let p_list = list_of_list [] (Classes.components_required cls ros) (Classes.sections cls ros) cls ros in
  {
    course_id = cls;
    permutations = generate_perms [] p_list
  }

(** [list_of_perm l r c] is a list of all class permutations of type
    [permutation], from list of class id's [c] of roster [r], appended to
    list [l]. *)
let rec list_of_perm p_list ros clses =
  match clses with
  | [] -> p_list
  | h::t -> list_of_perm ((class_permutations h ros)::p_list) ros t

let schedule_maker ros =
  let triple_list = ros |> Classes.course_ids |> list_of_perm [] ros |> perm_to_sched [] in
  List.map (fun x->
      begin
        List.fold_left (fun init y->
            begin
              match y with | (a,b) ->
                List.fold_left (fun init2 z-> add_section z a ros init2) init b
            end
          ) empty x
      end
    ) triple_list