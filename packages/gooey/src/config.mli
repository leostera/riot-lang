(** Configuration for layout computation *)
open Std

(** Available content-box space provided during measurement. *)
type constraints = {
  available_width: float option;
  available_height: float option;
}
(** Measured text size plus the wrapped lines that should be rendered. *)
type text_measurement = {
  size: Viewport.t;
  lines: string list;
}
(** Function type for measuring text dimensions. *)
type text_measurer = constraints:constraints -> string -> Style.t -> text_measurement
type t = {
  viewport: Viewport.t;
  text_measurer: text_measurer;
}

val constraints: ?available_width:float -> ?available_height:float -> unit -> constraints

val make: viewport:Viewport.t -> text_measurer:text_measurer -> unit -> t

val default_text_measurer: text_measurer

(**
   Terminal-cell-based text measurement.
   Width is the maximum visible line width and height is the rendered line count.
   The built-in measurer applies Gooey's wrapping rules and returns the wrapped lines.
*)
