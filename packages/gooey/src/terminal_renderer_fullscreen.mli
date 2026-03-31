(** Fullscreen terminal renderer with absolute positioning
    
    Use this renderer for full-screen/alternate screen applications where
    you have complete control of the terminal. Outputs ANSI escape sequences
    with absolute cursor positions (e.g., \x1b[1;1H for row 1, col 1).
*)
open Std

val render_to_string: Render.command_list -> string

(** Convert render commands to ANSI string with absolute cursor positioning *)
val render: Render.command_list -> unit

(** Print render commands directly to stdout *)
