(** ANSI formatting for terminal output *)
open Std
open Tty

type format =
  | Reset
  | Bold
  | Faint
  | Italic
  | Underline
  | Blink
  | Reverse
  | CrossOut
  | Overline
  | Foreground of Color.t
  | Background of Color.t

val to_string: format -> string

(** Convert a format to its ANSI escape sequence *)
val format_string: format list -> string -> string

(**
   [format_string formats text] applies ANSI formatting to text.

   Example:
   {[
     let red = Tty.Color.from_rgb (255, 0, 0) in
     format_string [Foreground red; Bold] "Hello"
   ]}
*)
