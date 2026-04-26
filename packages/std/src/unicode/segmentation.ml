(** Text segmentation - word, sentence, and line break detection *)
open Prelude
open Collections

module String = Kernel.String

(** Word boundary detection using simplified UAX #29 *)
let find_word_boundaries = fun s -> Word_break.find_word_boundaries s

(** Find the next word boundary after position pos *)
let find_next_word_start = fun s pos -> Word_break.find_next_word_start s pos

(** Find the previous word boundary before position pos *)
let find_prev_word_start = fun s pos -> Word_break.find_prev_word_start s pos

let find_sentence_boundaries = fun s ->
  (* Simplified: break on . ! ? *)
  let rec find pos acc =
    if pos >= String.length s then
      List.reverse acc
    else
      match String.get s ~at:pos with
      | Some char when List.contains [ '.'; '!'; '?' ] ~value:char ->
          find (pos + 1) ((pos + 1) :: acc)
      | _ -> find (pos + 1) acc
  in
  find 0 []

type line_break = Line_break.break_opportunity =
  | Must_break
  | Can_break
  | Dont_break

let find_line_breaks = fun s -> Line_break.find_line_breaks s

let wrap_lines = fun ~width s -> Line_break.wrap_lines ~width s
