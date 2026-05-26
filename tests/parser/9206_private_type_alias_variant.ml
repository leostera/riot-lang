(* Private type alias with variant representation *)

type color = Tty.Color.t =
  private | RGB of int * int * int
  | ANSI of int
  | ANSI256 of int
  | No_color
