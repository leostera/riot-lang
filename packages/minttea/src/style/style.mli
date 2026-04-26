type color = Tty.Color.t =
  private | RGB of int * int * int
  | ANSI of int
  | ANSI256 of int
  | No_color
val color: ?profile:Tty.Profile.t -> string -> color

val gradient: start:color -> finish:color -> steps:int -> color array

module Border: sig
  type t
  val make:
    ?top:string ->
    ?left:string ->
    ?bottom:string ->
    ?right:string ->
    ?top_left:string ->
    ?top_right:string ->
    ?bottom_left:string ->
    ?bottom_right:string ->
    ?middle_left:string ->
    ?middle_right:string ->
    ?middle:string ->
    ?middle_top:string ->
    ?middle_bottom:string ->
    unit ->
    t

  val normal: t

  val rounded: t

  val block: t

  val outer_half_block: t

  val inner_half_block: t

  val thick: t

  val double: t

  val hidden: t
end

(** Size specification for layout system *)
type size =
  | Auto
  (** Measure content, use intrinsic size *)
  | Fixed of int
  (** Explicit size in cells *)
  | Flex of float

(** Flexible unit, shares remaining space *)

(** Overflow behavior *)
type overflow =
  | Visible
  (** Don't clip (default) *)
  | Hidden
  (** Clip content that exceeds bounds *)
  | Scroll

(** Future: scrollable (not implemented yet) *)

(** Constraints for Auto/Flex sizing *)
type constraints = {
  min_width: int option;
  max_width: int option;
  min_height: int option;
  max_height: int option;
}
type t
val default: t

val equal: t -> t -> bool

val bg: color -> t -> t

val blink: bool -> t -> t

val bold: bool -> t -> t

val faint: bool -> t -> t

val fg: color -> t -> t

val italic: bool -> t -> t

val margin_bottom: int -> t -> t

val margin_left: int -> t -> t

val margin_right: int -> t -> t

val margin_top: int -> t -> t

val padding_bottom: int -> t -> t

val padding_left: int -> t -> t

val padding_right: int -> t -> t

val padding_top: int -> t -> t

val reverse: bool -> t -> t

val strikethrough: bool -> t -> t

val underline: bool -> t -> t

val border: Border.t -> t -> t

(** Legacy size API - kept for compatibility *)
val height: int -> t -> t

val width: int option -> t -> t

val max_height: int -> t -> t

val max_width: int -> t -> t

(** New size API - preferred for layout system *)
val width_auto: t -> t

(** Set width to Auto (intrinsic/content size) *)
val width_fixed: int -> t -> t

(** Set width to a fixed size in cells *)
val width_flex: float -> t -> t

(** Set width to flex with given weight (e.g. 1.0 for equal sharing) *)
val height_auto: t -> t

(** Set height to Auto (intrinsic/content size) *)
val height_fixed: int -> t -> t

(** Set height to a fixed size in cells *)
val height_flex: float -> t -> t

(** Set height to flex with given weight (e.g. 1.0 for equal sharing) *)

(** Constraint API *)
val min_width: int -> t -> t

(** Set minimum width constraint *)
val min_height: int -> t -> t

(** Set minimum height constraint *)

(** Overflow API *)
val overflow: overflow -> t -> t

(** Set overflow behavior (Visible, Hidden, or Scroll) *)
val align_horizontal: [`Left | `Center | `Right] -> t -> t

(**
   [align_horizontal pos t] sets horizontal text alignment.

   Only applies when [width] is set. Text will be padded to reach the target width.

   - [`Left] - Align left, pad right
   - [`Center] - Center text
   - [`Right] - Align right, pad left

   Example:
   ```ocaml
   let t = default
     |> width (Some 20)
     |> align_horizontal `Center
     |> fg (color "cyan") in
   render t "Hello"
   (* Renders:      Hello       *)
   ```
*)
val align_vertical: [`Top | `Center | `Bottom] -> t -> t

(**
   `align_vertical pos t` sets vertical text alignment.

   Only applies when `height` is set. Content will be padded with empty lines.

   - `` `Top`` - Align to top, pad bottom
   - `` `Center`` - Center content vertically
   - `` `Bottom`` - Align to bottom, pad top

   Example:
   ```ocaml
   let t = default
     |> height 5
     |> align_vertical `Center
     |> border Border.rounded in
   render t "Middle"
   (* Renders centered in 5-line box *)
   ```
*)
val render: t -> string -> string

(**
   `render t text` applies the t to text and returns formatted string.

   Processing order: padding, horizontal alignment, vertical alignment,
   text formatting (colors, bold, etc), borders, margins, max constraints

   Example:
   ```ocaml
   let td = default
     |> fg (color "green")
     |> bg (color "black")
     |> bold true
     |> padding_left 2
     |> padding_right 2
     |> border Border.rounded
     |> render in
   td "Hello World"
   ```
*)

(** Accessors for layout system *)
val get_padding_left: t -> int

val get_padding_right: t -> int

val get_padding_top: t -> int

val get_padding_bottom: t -> int

val get_width: t -> size

val get_height: t -> size

(** Accessors for rendering system *)
val get_foreground: t -> color option

val get_background: t -> color option

val get_bold: t -> bool

val get_italic: t -> bool

val get_underline: t -> bool

val get_strikethrough: t -> bool

val get_reverse: t -> bool
