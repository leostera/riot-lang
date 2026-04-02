open Std

type entry =
  | ML of string * Path.t
  | MLI of string * Path.t
  | C of string * Path.t
  | H of string * Path.t
  | Other of string * Path.t * string
  | Dir of string * Path.t * entry list
val scan: root:Path.t -> source_dir:Path.t -> entry list
