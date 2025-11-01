(** Text segmentation - word, sentence, and line break detection *)

(** Word boundary detection using simplified UAX #29 *)
let find_word_boundaries s =
  Word_break.find_word_boundaries s

(** Find the next word boundary after position pos *)
let find_next_word_start s pos =
  Word_break.find_next_word_start s pos

(** Find the previous word boundary before position pos *)
let find_prev_word_start s pos =
  Word_break.find_prev_word_start s pos

let find_sentence_boundaries s =
  (* Simplified: break on . ! ? *)
  let rec find pos acc =
    if pos >= Stdlib.String.length s then Stdlib.List.rev acc
    else if Stdlib.List.mem (Stdlib.String.get s pos) ['.'; '!'; '?'] then
      find (pos + 1) ((pos + 1) :: acc)
    else find (pos + 1) acc
  in
  find 0 []

type line_break = Line_break.break_opportunity =
  | Must_break
  | Can_break
  | Dont_break

let find_line_breaks s =
  Line_break.find_line_breaks s

let wrap_lines ~width s =
  Line_break.wrap_lines ~width s
