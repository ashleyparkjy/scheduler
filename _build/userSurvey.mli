(** 
   Answer collection for user survey questions.
*)

(** The abstract type of values representing user survey answers. *)
type t

(** The abstract type of class id.*)
type class_id

(** The output type of UserSurvey. *)
type t_output = {
  final_semester: string;
  final_classes: (string * string) list;
  classtime: (int * int);
  lunch_output: string;
  spread_output: string;
}

(** [init_state] is the initial state of survey response where the answers to 
    all of the questions are set as empty *)
val init_state : t

(** [get_classes st] is an association list of classes for current state [st]. *)
val get_classes : t -> (string * string) list 

(** [get_class_time st] is get the classtime_input from [st] and converts it
    to a tuple of two times. *)
val get_class_time: t -> (int*int)

(** [final_output st] converts [st] of type [t] to type [t_output]. *)
val final_output : t -> t_output

(** [is_valid_sem tl] is true if both the letter part and the number part of 
    the input [tl] has the correct format. Otherwise, it is false. 
    This will be checked through check_sem and check_num. *)
val is_valid_sem : string list -> bool

(** [is_valid_class tl] is true if the inputted class is a valid format of class
    name. It is false otherwise. *)
val is_valid_class : string list -> bool

(** [take_sem st tl] is the user attempting to take a semester input [tl] as 
    their answer to the survey question. If the user has not answered the 
    question yet and has an empty string as one's answer, the user will take 
    the content of [tl] as the input. Otherwise, the user will have the same
    previous answer as the input and ignore the incoming one. *)
val take_sem : t -> string list -> t

(** [delete_sem st tl] is the user attempting to delete the previous input [tl]
    for the semester prompt. If the user is attempting to delete the 
    pre-existing input, then the user will successfully empty out the reponse 
    for this particular question. Otherwise, the input will remain in the state,
    ignoring the incoming delete command. *)
val delete_sem : t -> string list -> t

(** [take_class st tl] is the user attempting to take a class [tl] as their
    answer to the survey question. The string list of one class (ex. ["CS";"3110"])
    is concatenated to the classes_input of [st]. *)
val take_class : t -> string list -> t

(** [delete_class st tl] is the user attempting to delete a class [tl] as their
    answer to the survey question. The classes_input of [st] is a new string 
    list list that has all the elements except [tl]. *)
val delete_class : t -> string list -> t

(** [take_class_time st tl] is the user attempting to take a class time [tl] as 
    their answer to the survey question. The string list of starting times of
    first and last classes (ex. ["10:10";"16:00"]) is concatenated to the 
    classtime_input of [st]. *)
val take_class_time : t -> string list -> t

(** [delete_class_time st] is the user attempting to delete what has been
    inputted as their answer to the survey question. The classtime_input of 
    [st] is a tuple of two empty strings. *)
val delete_class_time : t -> t

(** [is_valid_class_time tl] checks if both elements in the string list [tl] have
    valid formats of time. It is true if both satisfy the following properties:
      1. The string consists of numbers and : (2 by 2 separated by colon).
      2. The first two numbers range from 00 to 23 and the last two numbers 
      range from 00 to 59.
      3. The second time needs to be greater than the first time. *)
val is_valid_class_time : string list -> bool

(** [prompt_lunch st] prompts user to answer question on class spread over the week 
    and updates corresponding information to [st]. It also handles any commands 
    that are written. *)
val prompt_spread : t -> t

(** [prompt_lunch st] prompts user to answer flexibility in lunch time question 
    and updates corresponding information to [st]. It also handles any commands 
    that are written. *)
val prompt_lunch : t -> t

(** [prompt_class_time st] prompts user to answer preferred class start and end times 
    in the day and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
val prompt_class_time : t -> t

(** [prompt_class st] prompts user to answer class question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
val prompt_class : t -> t

(** [prompt_semester st] prompts user to answer semester question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
val prompt_semester : t -> t




