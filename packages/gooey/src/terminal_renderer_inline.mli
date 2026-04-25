(**
   Inline terminal renderer with line-by-line output

   Use this renderer for inline/non-fullscreen applications that render content
   within the normal terminal flow. Outputs newline-separated lines with ANSI
   colors and erase-to-end-of-line sequences.
*)
open Std

val render_to_string: Render.command_list -> string

(** Convert render commands to ANSI string with line-by-line output *)
val render: Render.command_list -> unit(** Print render commands directly to stdout *)
