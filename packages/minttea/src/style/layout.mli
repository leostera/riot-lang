(** Layout composition utilities.

    This module provides functions for composing and positioning text blocks
    in TUI layouts, similar to Lipgloss's layout system.

    ## Example: Horizontal Layout

    ```ocaml
    open Std
    open Minttea

    let left = "Left\nPanel"
    let right = "Right\nPanel"
    let joined = Layout.join_horizontal ~pos:`Top [left; right]
    (* Result:
       Left  Right
       PanelPanel
    *)
    ```

    ## Example: Vertical Layout

    ```ocaml
    let header = "Header"
    let content = "Content line"
    let layout = Layout.join_vertical ~pos:`Left [header; content]
    (* Result:
       Header      
       Content line
    *)
    ```

    ## Example: Positioning

    ```ocaml
    let text = "Centered"
    let positioned = Layout.place 
      ~width:20 ~height:5
      ~h_pos:0.5 ~v_pos:0.5 
      text
    (* Centers text in a 20x5 box *)
    ``` *)

(** ## Horizontal Composition *)

val join_horizontal : pos:[`Top | `Center | `Bottom] -> string list -> string
(** `join_horizontal ~pos strs` places strings side-by-side horizontally.
    
    - Calculates the maximum height across all strings
    - Pads shorter strings vertically according to `pos`:
      - `` `Top`` - Align to top, pad bottom
      - `` `Center`` - Center vertically
      - `` `Bottom`` - Align to bottom, pad top
    - Joins strings left-to-right
    
    Preserves ANSI formatting in all strings. *)

(** ## Vertical Composition *)

val join_vertical : pos:[`Left | `Center | `Right] -> string list -> string
(** `join_vertical ~pos strs` stacks strings vertically.
    
    - Calculates the maximum width across all strings
    - Pads narrower strings horizontally according to `pos`:
      - `` `Left`` - Align to left, pad right
      - `` `Center`` - Center horizontally
      - `` `Right`` - Align to right, pad left
    - Joins strings top-to-bottom
    
    Preserves ANSI formatting in all strings. *)

(** ## Absolute Positioning *)

val place : 
  width:int -> 
  height:int -> 
  h_pos:float -> 
  v_pos:float -> 
  string -> 
  string
(** `place ~width ~height ~h_pos ~v_pos str` positions `str` within a box.
    
    - Creates a box of `width` x `height` filled with spaces
    - Positions `str` using fractional coordinates:
      - `h_pos`: 0.0 = left, 0.5 = center, 1.0 = right
      - `v_pos`: 0.0 = top, 0.5 = center, 1.0 = bottom
    - Useful for creating centered dialogs, splash screens, etc.
    
    Example:
    ```ocaml
    (* Top-left corner *)
    place ~width:10 ~height:5 ~h_pos:0.0 ~v_pos:0.0 "TL"
    
    (* Dead center *)
    place ~width:10 ~height:5 ~h_pos:0.5 ~v_pos:0.5 "Center"
    
    (* Bottom-right *)
    place ~width:10 ~height:5 ~h_pos:1.0 ~v_pos:1.0 "BR"
    ``` *)
