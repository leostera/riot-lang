(* Generated interface for example-lib *)

(** Create a new point *)
val point_new : int -> int -> unknown

(** Get the x coordinate of a point *)
val point_x : unknown -> int

(** Get the y coordinate of a point *)
val point_y : unknown -> int

(** Calculate distance between two points *)
val point_distance : unknown -> unknown -> float

(** Free a point *)
val point_free : unknown -> unit

(** Add two numbers *)
val add : int -> int -> int

(** Multiply two numbers *)
val multiply : int -> int -> int

(** Square a number *)
val square : int -> int

