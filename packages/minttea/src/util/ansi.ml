open Std

type ansi_state = {
  bold : bool;
  italic : bool;
  underline : bool;
  fg_color : string option;
  bg_color : string option;
}

(* Strip all ANSI escape sequences *)

let strip = Tty.Escape_seq.strip

(* Calculate display width ignoring ANSI codes *)

let width = Tty.Escape_seq.width

(* Split string into lines *)

let split_lines = fun str ->
  String.split_on_char '\n' str

(* Pad functions *)

let pad_right = fun ~width:target_width c str ->
  let w = width str in
  if w >= target_width then
    str
  else
    str ^ String.make (target_width - w) c

let pad_left = fun ~width:target_width c str ->
  let w = width str in
  if w >= target_width then
    str
  else
    String.make (target_width - w) c ^ str

let pad_center = fun ~width:target_width c str ->
  let w = width str in
  if w >= target_width then
    str
  else
    let total_pad = target_width - w in
    let left_pad = total_pad / 2 in
    let right_pad = total_pad - left_pad in
    String.make left_pad c ^ str ^ String.make right_pad c

(* Truncate with ellipsis, preserving ANSI codes *)

let truncate = fun ~width ?ellipsis:tail str -> String.truncate_width ~width ?tail str

(* Helper to check if string contains substring *)

let contains_substring = fun haystack needle ->
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len = 0 then
    true
  else if n_len > h_len then
    false
  else
    let rec search = fun i ->
      if i > h_len - n_len then
        false
      else
        let rec match_at = fun j ->
          if j = n_len then
            true
          else if haystack.[i + j] = needle.[j] then
            match_at (j + 1)
          else
            false
        in
        if match_at 0 then
          true
        else
          search (i + 1)
    in
    search 0

(* Parse ANSI state from string (simplified) *)

let parse_state = fun str ->
  {
    bold = contains_substring str "\027[1m";
    italic = contains_substring str "\027[3m";
    underline = contains_substring str "\027[4m";
    fg_color = None;
    bg_color = None;

  }

(* Convert state to ANSI codes *)

let state_to_codes = fun state ->
  let codes = [] in
  let codes =
    if state.bold then
      "1" :: codes
    else
      codes
  in
  let codes =
    if state.italic then
      "3" :: codes
    else
      codes
  in
  let codes =
    if state.underline then
      "4" :: codes
    else
      codes
  in
  let codes =
    match state.fg_color with
    | Some c -> ("38;" ^ c) :: codes
    | None -> codes
  in
  let codes =
    match state.bg_color with
    | Some c -> ("48;" ^ c) :: codes
    | None -> codes
  in
  if codes = [] then
    ""
  else
    "\027[" ^ String.concat ";" codes ^ "m"

(* Word wrapping with ANSI preservation *)

let word_wrap = fun ~width:target_width str ->
  if target_width <= 0 then
    [ str ]
  else
    let lines = split_lines str in
    let wrap_line = fun line ->
      if width line <= target_width then
        [ line ]
      else
        (* Split into words while tracking ANSI codes *)
        let words = String.split_on_char ' ' line in
        let rec build_lines = fun current_line current_width acc ->
          function
          | [] ->
              if current_line = "" then
                List.rev acc
              else
                List.rev (current_line :: acc)
          | word :: rest ->
              let word_width = width word in
              let space_width =
                if current_line = "" then
                  0
                else
                  1
              in
              let new_width = current_width + space_width + word_width in
              if current_line = "" then
                if word_width > target_width then
                  let stripped_word = strip word in
                  let chars_fit = target_width in
                  if chars_fit <= 0 then
                    build_lines "" 0 (word :: acc) rest
                  else
                    let part = String.sub
                    stripped_word
                    0
                    (min chars_fit (String.length stripped_word)) in
                    let remaining = String.sub
                    stripped_word
                    chars_fit
                    (String.length stripped_word - chars_fit) in
                    build_lines "" 0 (part :: acc) (remaining :: rest)
                else
                  build_lines word word_width acc rest
              else if new_width <= target_width then
                build_lines (current_line ^ " " ^ word) new_width acc rest
              else
                (* Start new line with this word *)
                build_lines word word_width (current_line :: acc) rest
        in
        build_lines "" 0 [] words
    in
    List.concat (List.map wrap_line lines)
