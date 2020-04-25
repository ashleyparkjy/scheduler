(** 
   Answer collection for user survey questions.
*)

(** The abstract type of values representing user survey answers. *)
type t

(** The abstract type of class id.*)
type class_id

(** The output type of UserSurvey. *)
type t_output

(** [get_semester st] is a string of semester for current state [st]. *)
val get_semester : t -> string

(** [get_classes st] is an association list of classes for current state [st]. *)
val get_classes : t -> (string * string) list 

(** [final_output st] is a record with semester of tuple list of classes 
    inputted in final state [st]*)
val final_output : t -> t_output

(** [init_state] is the initial state of survey response where the answers to 
    all of the questions are set as empty *)
val init_state : t

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

(** [prompt_routine st] prompts user to answer routine question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
val prompt_routine : t -> unit

(** [prompt_class st] prompts user to answer class question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
val prompt_class : t -> unit

(** [prompt_semester st] prompts user to answer semester question and updates 
    corresponding information to [st]. It also handles any commands that are written. *)
val prompt_semester : t -> unit

