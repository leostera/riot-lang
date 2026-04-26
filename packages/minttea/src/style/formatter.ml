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

let to_string = fun fmt ->
  match fmt with
  | Reset -> Escape_seq.reset_seq
  | Bold -> Escape_seq.bold_seq
  | Faint -> Escape_seq.faint_seq
  | Italic -> Escape_seq.italics_seq
  | Underline -> Escape_seq.underline_seq
  | Blink -> Escape_seq.blink_seq
  | Reverse -> Escape_seq.reverse_seq
  | CrossOut -> Escape_seq.cross_out_seq
  | Overline -> Escape_seq.overline_seq
  | Foreground color -> Escape_seq.foreground_seq ^ ";" ^ Color.to_escape_seq ~mode:`fg color
  | Background color -> Escape_seq.background_seq ^ ";" ^ Color.to_escape_seq ~mode:`bg color

let format_string = fun seqs line ->
  let seqs =
    List.map ~fn:to_string seqs
    |> String.concat ";"
  in
  if seqs = "" then
    line
  else
    Escape_seq.csi ^ seqs ^ "m" ^ line ^ Escape_seq.csi ^ Escape_seq.reset_seq ^ "m"
