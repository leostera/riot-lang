(** Configuration for layout computation *)

open Std

type text_measurer = string -> Style.t -> Viewport.t
(** Function type for measuring text dimensions *)

type t = {
  viewport : Viewport.t;
  text_measurer : text_measurer;
}

val make : 
  viewport:Viewport.t ->
  text_measurer:text_measurer ->
  unit ->
  t

val default_text_measurer : text_measurer
(** Simple terminal-based text measurement *)
