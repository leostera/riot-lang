open Global

(** Metadata attached to log events *)

type t = {
  module_name : string option;
  function_name : string option;
  file : string option;
  line : int option;
  pid : Pid.t option;
  custom : (string * string) list;
}

val empty : t
(** Empty metadata with all fields set to None *)

val make :
  ?module_name:string ->
  ?function_name:string ->
  ?file:string ->
  ?line:int ->
  ?pid:Pid.t ->
  ?custom:(string * string) list ->
  unit ->
  t
(** Create metadata with the given fields *)

val merge : t -> t -> t
(** Merge two metadata records. The second argument takes precedence for
    conflicting fields, and custom fields are concatenated. *)

val to_string : t -> string
(** Convert metadata to a string representation for display *)
