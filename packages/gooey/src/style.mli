(** Style configuration for layout elements *)
open Std

type color = Colors.rgb

(** Terminal RGB color used throughout Gooey's public API. *)

(** Layout direction *)
type direction =
  | LeftToRight
  | TopToBottom
(** Sizing type for width/height *)
type sizing_type =
  | Fit
  (** Fit to content size *)
  | Grow
  (** Grow to fill available space *)
  | Fixed of float
  (** Fixed size in terminal cells *)
  | Percent of float

(** Percentage of parent (0.0-1.0) *)

(** Sizing configuration *)
type sizing = {
  width: sizing_type;
  height: sizing_type;
  min_width: float option;
  max_width: float option;
  min_height: float option;
  max_height: float option;
}
(** Horizontal alignment *)
type h_align =
  | Left
  | Center
  | Right
(** Vertical alignment *)
type v_align =
  | Top
  | Middle
  | Bottom
(** Alignment configuration *)
type alignment = {
  x: h_align;
  y: v_align;
}
(** Padding *)
type padding = { left: int; right: int; top: int; bottom: int }
(** Margin *)
type margin = { left: int; right: int; top: int; bottom: int }
(** Overflow behavior for child content *)
type overflow =
  | Visible
  | Clip
(** Text wrapping mode *)
type text_wrap =
  | Words
  (** Wrap on word boundaries *)
  | NoWrap
  (** No wrapping *)
  | Character

(** Wrap on character boundaries *)

(** Text alignment *)
type text_align =
  | TextLeft
  | TextCenter
  | TextRight
(** Font weight *)
type font_weight =
  | Normal
  | Bold
(** Text decoration *)
type text_decoration =
  | NoDecoration
  | Underline
  | Strikethrough
(** Corner radius for borders *)
type corner_radius = { top_left: float; top_right: float; bottom_left: float; bottom_right: float }
(** Complete style configuration *)
type t = {
  (* Layout properties *)
  direction: direction;
  sizing: sizing;
  grow_weight: float;
  alignment: alignment;
  child_gap: int;
  padding: padding;
  margin: margin;
  overflow: overflow;
  (* Visual properties *)
  background: color option;
  foreground: color option;
  border_width: int;
  border_color: color option;
  corner_radius: corner_radius;
  (* Text properties *)
  text_size: int;
  text_wrap: text_wrap;
  text_align: text_align;
  font_weight: font_weight;
  text_decoration: text_decoration;
  (* Z-index for layering *)
  z_index: int;
}

(** Empty/default style *)
val empty: t

val row: t -> t

(** Set direction to LeftToRight *)
val column: t -> t

(** Set direction to TopToBottom *)
val size: width:sizing_type -> height:sizing_type -> t -> t

(** Set width and height sizing *)
val width: sizing_type -> t -> t

(** Set width sizing *)
val height: sizing_type -> t -> t

(** Set height sizing *)
val min_width: float -> t -> t

val max_width: float -> t -> t

val min_height: float -> t -> t

val max_height: float -> t -> t

val padding: padding -> t -> t

val margin: margin -> t -> t

val overflow: overflow -> t -> t

val clip: t -> t

val bg: color -> t -> t

(** Set background color *)
val fg: color -> t -> t

(** Set foreground color *)
val border: ?width:int -> ?color:color -> ?radius:corner_radius -> unit -> t -> t

(** Set border properties *)
val text_size: int -> t -> t

(** Terminal renderers currently treat text_size as metadata only. *)
val bold: t -> t

val underline: t -> t

val align: x:h_align -> y:v_align -> t -> t

val align_left: t -> t

val align_center: t -> t

val align_right: t -> t

val grow_weight: float -> t -> t

(** Set the weight used when distributing remaining space to Grow children *)
val grow: t -> t

(** Set both width and height to Grow *)
val fixed: width:float -> height:float -> t -> t

(** Set both width and height to Fixed *)
val text_wrap: text_wrap -> t -> t

val text_align: text_align -> t -> t

val child_gap: int -> t -> t

val z_index: int -> t -> t

val strikethrough: t -> t

module Padding: sig
  val make: ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> unit -> padding

  val all: int -> padding

  val symmetric: h:int -> v:int -> padding

  val empty: padding
end

module Margin: sig
  val make: ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> unit -> margin

  val all: int -> margin

  val symmetric: h:int -> v:int -> margin

  val empty: margin
end

module CornerRadius: sig
  val make:
    ?top_left:float ->
    ?top_right:float ->
    ?bottom_left:float ->
    ?bottom_right:float ->
    unit ->
    corner_radius

  val all: float -> corner_radius

  val zero: corner_radius
end

val color: string -> color

(**
   [color hex] parses a hex color string like "#FF0000" or "#F00" into an RGB color.

   Examples:
   - [color "#FF0000"] returns a red color
   - [color "#F00"] returns a red color

   @raise Invalid_argument if the hex string is malformed
*)
