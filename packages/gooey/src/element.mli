(** UI element tree *)
open Std

(** Element type - represents a UI element in the tree *)
type t =
  | Text of { style: Style.t; content: string; }
  | Container of { style: Style.t; children: t list; }
  | Empty
  | Custom of {
      style: Style.t;
      measure: unit -> Viewport.t;
      render: Geometry.Rect.t -> Render.command list;
    }
(** Custom elements for things like images, charts, or other custom rendering.
      The measure function tells the layout engine how big this element wants to be.
      The render function receives the final bounding box and returns render commands. *)
(** {1 Element Constructors} *)

val text: ?style:Style.t -> string -> t

(** Create a text element *)
val container: ?style:Style.t -> t list -> t

(** Create a container element with children *)
val empty: t

(** Empty element (takes no space) *)
val custom:
  ?style:Style.t ->
  measure:(unit -> Viewport.t) ->
  render:(Geometry.Rect.t -> Render.command list) ->
  unit ->
  t

(** Create a custom element *)
(** {1 Common Layouts} *)

val row: ?style:Style.t -> t list -> t

(** Container with LeftToRight direction *)
val column: ?style:Style.t -> t list -> t

(** Container with TopToBottom direction *)
val spacer: ?flex:float -> unit -> t

(** Empty container that grows to fill space *)
