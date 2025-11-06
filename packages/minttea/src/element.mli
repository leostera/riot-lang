(** Declarative layout system for Minttea.
    
    The Element module provides a declarative way to build TUI layouts without
    manual size calculations. It uses a flex-based layout algorithm inspired by
    CSS Flexbox and SwiftUI.
    
    ## Core Concept
    
    Elements form a tree structure where each element can have:
    - A style (for colors, borders, padding, etc.)
    - A size specification (Auto, Fixed, or Flex)
    - Child elements (for Row/Column containers)
    
    The layout engine automatically calculates dimensions based on:
    1. Fixed sizes (explicit dimensions)
    2. Auto sizes (measured from content)
    3. Flex sizes (share remaining space proportionally)
    
    ## Example: Basic Layout
    
    ```ocaml
    open Minttea
    open Element
    
    let my_view model =
      column [
        (* Header: fixed height *)
        text ~style:(Style.default 
          |> Style.height_fixed 3
          |> Style.bg (Style.color "#0064FF"))
          "My App";
        
        (* Content: flexible *)
        text ~style:(Style.default |> Style.height_flex 1.0)
          model.content;
        
        (* Footer: fixed height *)
        text ~style:(Style.default |> Style.height_fixed 1)
          "Status: Ready";
      ]
      |> render_to_size (Tty.Size.make ~cols:80 ~rows:24)
    ```
    
    ## Example: Two-Column Layout
    
    ```ocaml
    let two_column_view left_content right_content =
      row [
        (* Left: 1/3 width *)
        box ~style:(Style.default |> Style.width_flex 1.0)
          (text left_content);
        
        (* Spacer: fixed gap *)
        h_space 2;
        
        (* Right: 2/3 width *)
        box ~style:(Style.default |> Style.width_flex 2.0)
          (text right_content);
      ]
    ```
    
    ## Design Principles
    
    1. **Every element has a style** - Consistent with HTML/CSS
    2. **No gap parameter** - Use spacer elements instead (more flexible)
    3. **Flex units** - Like CSS Grid `1fr`, `2fr` for proportional sizing
    4. **Auto for intrinsic** - Measures content automatically
    5. **Simple algorithm** - One-pass layout, no constraint solver *)

(** Position specification for layers *)
type position =
  | Relative  (* Normal flow positioning *)
  | Absolute of int * int  (* Absolute positioning at (x, y) *)

(** Core element type - represents a node in the layout tree *)
type t =
  | Text of Style.t * string
  | Box of Style.t * t
  | Row of Style.t * t list
  | Column of Style.t * t list
  | Layer of Style.t * position * t list  (* Stack elements with positioning *)
  | Spacer of Style.t
  | Empty

(** Structural equality for elements *)
val equal : t -> t -> bool

(** ## Basic Elements *)

val text : ?style:Style.t -> string -> t
(** Create a text element.
    
    Text is the leaf node of the layout tree. By default, text has Auto sizing
    which means it will measure to fit its content.
    
    Example:
    ```ocaml
    text "Hello World"
    text ~style:(Style.default |> Style.fg (Style.color "cyan")) "Colored"
    ``` *)

val box : ?style:Style.t -> t -> t
(** Wrap an element in a box with optional styling.
    
    Boxes are useful for adding padding, borders, background colors, etc.
    
    Example:
    ```ocaml
    box ~style:(Style.default 
      |> Style.border Style.Border.rounded
      |> Style.padding_left 2)
      (text "Boxed content")
    ``` *)

val empty : t
(** Create an empty element (renders as nothing).
    
    Useful as a placeholder or conditional element.
    
    Example:
    ```ocaml
    if show_sidebar then sidebar else empty
    ``` *)

(** ## Container Elements *)

val row : ?style:Style.t -> t list -> t
(** Arrange children horizontally (left to right).
    
    Children are laid out along the horizontal axis. Each child's width is
    determined by its size specification (Auto, Fixed, or Flex).
    
    Example:
    ```ocaml
    row [
      text ~style:(Style.default |> Style.width_fixed 20) "Left";
      h_space 2;  (* 2-column gap *)
      text ~style:(Style.default |> Style.width_flex 1.0) "Right fills";
    ]
    ``` *)

val column : ?style:Style.t -> t list -> t
(** Arrange children vertically (top to bottom).
    
    Children are laid out along the vertical axis. Each child's height is
    determined by its size specification (Auto, Fixed, or Flex).
    
    Example:
    ```ocaml
    column [
      text ~style:(Style.default |> Style.height_fixed 3) "Header";
      text ~style:(Style.default |> Style.height_flex 1.0) "Content grows";
      text ~style:(Style.default |> Style.height_fixed 1) "Footer";
    ]
    ``` *)

val layer : ?style:Style.t -> ?pos:position -> t list -> t
(** Stack children with optional absolute positioning.
    
    Layers allow you to position elements absolutely or stack them with z-indexing.
    Each child in the layer gets its own z-index (later children appear on top).
    
    Example:
    ```ocaml
    (* Absolute positioning *)
    layer ~pos:(Absolute (10, 5)) [
      text ~style:(Style.default |> Style.bg (Style.color "blue")) "  "
    ]
    
    (* Stacking multiple elements *)
    layer [
      text "Background";
      layer ~pos:(Absolute (5, 2)) [text "Overlay"];
    ]
    ``` *)

(** ## Spacing Elements *)

val spacer : ?style:Style.t -> unit -> t
(** Create a flexible spacer element.
    
    Spacers with Flex sizing expand to fill available space. Use them to create
    gaps, push elements apart, or center content.
    
    Example:
    ```ocaml
    (* Center an element *)
    row [
      spacer ~style:(Style.default |> Style.width_flex 1.0) ();
      text "Centered";
      spacer ~style:(Style.default |> Style.width_flex 1.0) ();
    ]
    ``` *)

val h_space : int -> t
(** Create a horizontal spacer with fixed width.
    
    Convenience for `spacer ~style:(Style.default |> Style.width_fixed n) ()`.
    
    Example:
    ```ocaml
    row [text "Left"; h_space 5; text "Right"]
    ``` *)

val v_space : int -> t
(** Create a vertical spacer with fixed height.
    
    Convenience for `spacer ~style:(Style.default |> Style.height_fixed n) ()`.
    
    Example:
    ```ocaml
    column [text "Top"; v_space 3; text "Bottom"]
    ``` *)

val h_flex : ?weight:float -> unit -> t
(** Create a flexible horizontal spacer (default weight: 1.0).
    
    Expands horizontally to fill available space in a row.
    
    Example:
    ```ocaml
    (* Push to right *)
    row [
      text "Left";
      h_flex ();  (* Fills available space *)
      text "Right";
    ]
    ``` *)

val v_flex : ?weight:float -> unit -> t
(** Create a flexible vertical spacer (default weight: 1.0).
    
    Expands vertically to fill available space in a column.
    
    Example:
    ```ocaml
    (* Push to bottom *)
    column [
      text "Top";
      v_flex ();  (* Fills available space *)
      text "Bottom";
    ]
    ``` *)

(** ## Convenience Functions *)

val spaced_row : ?gap:int -> ?style:Style.t -> t list -> t
(** Create a row with fixed spacing between children.
    
    This is a convenience function that inserts h_space elements between children.
    
    Example:
    ```ocaml
    spaced_row ~gap:2 [text "A"; text "B"; text "C"]
    (* Equivalent to: row [text "A"; h_space 2; text "B"; h_space 2; text "C"] *)
    ``` *)

val spaced_column : ?gap:int -> ?style:Style.t -> t list -> t
(** Create a column with fixed spacing between children.
    
    This is a convenience function that inserts v_space elements between children.
    
    Example:
    ```ocaml
    spaced_column ~gap:1 [text "Line 1"; text "Line 2"; text "Line 3"]
    ``` *)

