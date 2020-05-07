(**
   Metadata about this submission.
   @author Junho Kim-Lee (jk2333)
   @author Ashley Park (ap764)
   @author Kyu Hwan Choi (kc677)
   @attributions Makefile is based on the standard assignment Makefile from the
   CS3110 course staff. The structure of classes.ml is loosely based around
   adventure.ml from A2. The structure of command.ml is loosely based on the
   module from A2 with the same name. get_json in courseJson.ml is adapted from
   demo code of the Cohttp package from https://github.com/avsm/ocaml-cohttp.
   cmp_set_like_lists, pp_string, and pp_list are clones of the functions
   with the same name from the test.ml skeleton code written by the CS 3110
   staff.
*)

(** [hours_worked] is a list of the number of hours each team member
    worked on this assignment. *)
val hours_worked : int list