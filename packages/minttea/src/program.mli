open Std

(** Main program runtime for Minttea applications *)

type Message.t += Timer of Timer.id Ref.t
(** Program message types *)

type 'model t
(** A program with model type 'model *)

val make : app:'model App.t -> config:Config.t -> 'model t
(** Create a new program *)

val run : 'model t -> 'model -> (unit, string) result
(** Run the program with an initial model *)
