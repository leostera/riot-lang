(**
   Scrollable content area component.

   Displays a scrollable window into larger content. Essential
   for logs, help text, long lists, or any content that exceeds screen height.

   ## Example: Basic Viewport

   ```ocaml
   open Std
   open Minttea

   type model = { viewport : Viewport.t; content : string }

   let init () =
     let viewport = Viewport.make ~width:80 ~height:24 in
     let viewport = Viewport.set_content viewport "Very long content here..." in
     ({ viewport; content }, Command.Noop)

   let update event model =
     match event with
     | Event.KeyDown (Up, _) ->
         let viewport = Viewport.scroll_up model.viewport 1 in
         ({ model with viewport }, Command.Noop)
     | Event.KeyDown (Down, _) ->
         let viewport = Viewport.scroll_down model.viewport 1 in
         ({ model with viewport }, Command.Noop)
     | Event.KeyDown (Page_up, _) ->
         let viewport = Viewport.page_up model.viewport in
         ({ model with viewport }, Command.Noop)
     | Event.KeyDown (Page_down, _) ->
         let viewport = Viewport.page_down model.viewport in
         ({ model with viewport }, Command.Noop)
     | _ -> (model, Command.Noop)

   let view model =
     Viewport.view model.viewport
   ```
*)
open Std

(** A viewport instance. *)
type t
type wrap_mode = [`None | `Soft]

(**
   Text wrapping mode:
   - `` `None`` - No wrapping, lines can exceed viewport width
   - `` `Soft`` - Soft wrap at word boundaries to fit viewport width
*)
val make: width:int -> height:int -> t

(**
   `make ~width ~height` creates a new viewport with the given dimensions.

   The viewport starts at the top (y_offset = 0) with no content.
*)
val set_content: t -> content:string -> t

(**
   `set_content viewport ~content` sets the content to display.

   Content is split into lines. If current scroll position is past
   the new content's end, viewport scrolls to bottom.
*)
val get_content: t -> string

(** `get_content viewport` returns the full content (all lines joined). *)
val total_lines: t -> int

(** `total_lines viewport` returns the total number of lines in the content. *)
val visible_lines: t -> int

(** `visible_lines viewport` returns how many lines are currently visible. *)
val set_width: t -> width:int -> t

(** `set_width viewport ~width` changes the viewport width. *)
val set_height: t -> height:int -> t

(** `set_height viewport ~height` changes the viewport height. *)
val width: t -> int

(** `width viewport` returns the current width. *)
val height: t -> int

(** `height viewport` returns the current height. *)
val set_wrap_mode: t -> mode:wrap_mode -> t

(**
   `set_wrap_mode viewport ~mode` sets the text wrapping mode.

   - `` `None`` - Lines are not wrapped (default)
   - `` `Soft`` - Lines are word-wrapped to fit viewport width

   Example:
   ```ocaml
   let viewport = Viewport.make ~width:40 ~height:10
     |> Viewport.set_wrap_mode ~mode:`Soft
     |> Viewport.set_content ~content:"Very long message that exceeds width"
   (* Content will automatically wrap at word boundaries *)
   ```
*)
val wrap_mode: t -> wrap_mode

(** `wrap_mode viewport` returns the current wrap mode. *)
val y_offset: t -> int

(** `y_offset viewport` returns the current vertical scroll position (0-based). *)
val set_y_offset: t -> offset:int -> t

(**
   `set_y_offset viewport ~offset` sets the vertical scroll position.

   Automatically clamped to valid range `[0, max_offset]`.
*)
val scroll_up: t -> lines:int -> t

(**
   `scroll_up viewport ~lines` scrolls up by the given number of lines.

   Returns viewport unchanged if already at top.
*)
val scroll_down: t -> lines:int -> t

(**
   `scroll_down viewport ~lines` scrolls down by the given number of lines.

   Returns viewport unchanged if already at bottom.
*)
val page_up: t -> t

(** `page_up viewport` scrolls up by one viewport height ("page up"). *)
val page_down: t -> t

(** `page_down viewport` scrolls down by one viewport height ("page down"). *)
val half_page_up: t -> t

(** `half_page_up viewport` scrolls up by half a viewport height. *)
val half_page_down: t -> t

(** `half_page_down viewport` scrolls down by half a viewport height. *)
val goto_top: t -> t

(** `goto_top viewport` scrolls to the very top (y_offset = 0). *)
val goto_bottom: t -> t

(** `goto_bottom viewport` scrolls to the very bottom. *)
val at_top: t -> bool

(** `at_top viewport` returns true if scrolled to the very top. *)
val at_bottom: t -> bool

(** `at_bottom viewport` returns true if scrolled to or past the bottom. *)
val scroll_percent: t -> float

(**
   `scroll_percent viewport` returns scroll position as 0.0-1.0.

   - 0.0 = top
   - 1.0 = bottom
   - Values in between = proportional position
*)
val set_mouse_wheel_enabled: t -> enabled:bool -> t

(** `set_mouse_wheel_enabled viewport ~enabled` enables/disables mouse wheel scrolling. *)
val set_mouse_wheel_delta: t -> delta:int -> t

(** `set_mouse_wheel_delta viewport ~delta` sets lines per mouse wheel notch (default: 3). *)
val view: t -> string

(**
   `view viewport` renders the visible portion of the content.

   Returns only the lines that fit in the viewport at the current scroll position.
   Lines are joined with newlines.
*)
