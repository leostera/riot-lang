open Global
open Collections
open Iter

include Kernel.String

module MutIter = struct
  type state = {
    source: string;
    mutable current_pos: int;
  }

  type item = Uchar.t

  let next = fun state ->
    if state.current_pos < length state.source then
      (
        let utf_decoded = get_utf_8_uchar state.source state.current_pos in
        let char_size = Uchar.utf_decode_length utf_decoded in
        let item = Uchar.utf_decode_uchar utf_decoded in
        state.current_pos <- state.current_pos + char_size;
        Some item
      )
    else
      None

  let size = fun { current_pos; source } -> length source - current_pos

  let clone = fun { source; current_pos } -> { source; current_pos }
end

let into_mut_iter = fun source ->
  MutIterator.make (module MutIter) { source; current_pos = 0 }

module Iter = struct
  type state = {
    source: string;
    current_pos: int;
  }

  type item = Uchar.t

  let next = fun ({ source; current_pos } as state) ->
    if current_pos < length source then
      let utf_decoded = get_utf_8_uchar source current_pos in
      let char_size = Uchar.utf_decode_length utf_decoded in
      let item = Uchar.utf_decode_uchar utf_decoded in
      (Some item, { state with current_pos = current_pos + char_size })
    else
      (None, state)

  let size = fun { current_pos; source } -> length source - current_pos
end

let into_iter = fun source ->
  Iterator.make (module Iter) { source; current_pos = 0 }

(* Unicode-aware operations *)

let width = fun s ->
  (* Calculate display width by summing rune widths *)
  into_iter s |> Iterator.to_list |> List.fold_left (fun acc rune -> acc + Unicode.Rune.width rune) 0

let rune_count = fun s -> into_iter s |> Iterator.to_list |> List.length

let grapheme_count = fun s ->
  (* Count grapheme clusters *)
  let rec count pos acc =
    if pos >= length s then
      acc
    else
      match Unicode.Grapheme.first (sub s pos (length s - pos)) with
      | None -> acc
      | Some (_, rest) ->
          let consumed = length s - pos - length rest in
          count (pos + consumed) (acc + 1)
  in
  count 0 0

let truncate_width = fun ~width:target_width ?(tail = "…") s ->
  let s_width = width s in
  if s_width <= target_width then
    s
  else
    let tail_width = length tail in
    (* Simplified: assume ASCII tail *)
    let max_width = target_width - tail_width in
    if max_width <= 0 then
      tail
    else
      (* Find position where width exceeds target *)
      let rec find_cut pos acc_width =
        if pos >= length s then
          s
        else
          let decode = get_utf_8_uchar s pos in
          if Uchar.utf_decode_is_valid decode then
            let rune = Uchar.utf_decode_uchar decode in
            let rune_w = Unicode.Rune.width rune in
            if acc_width + rune_w > max_width then
              sub s 0 pos ^ tail
            else
              let len = Uchar.utf_decode_length decode in
              find_cut (pos + len) (acc_width + rune_w)
          else
            sub s 0 pos ^ tail
      in
      find_cut 0 0

let pad_left = fun ~width:target_width pad_char s ->
  let s_width = width s in
  if s_width >= target_width then
    s
  else
    let padding = make (target_width - s_width) pad_char in
    padding ^ s

let pad_right = fun ~width:target_width pad_char s ->
  let s_width = width s in
  if s_width >= target_width then
    s
  else
    let padding = make (target_width - s_width) pad_char in
    s ^ padding

let pad_center = fun ~width:target_width pad_char s ->
  let s_width = width s in
  if s_width >= target_width then
    s
  else
    let total_padding = target_width - s_width in
    let left_padding = total_padding / 2 in
    let right_padding = total_padding - left_padding in
    make left_padding pad_char ^ s ^ make right_padding pad_char

(* Grapheme iterators *)

module GraphemeMutIter = struct
  type state = {
    source: string;
    mutable current_pos: int;
  }

  type item = Unicode.Grapheme.t

  let next = fun state ->
    if state.current_pos < length state.source then
      let remaining = sub state.source state.current_pos (length state.source - state.current_pos) in
      match Unicode.Grapheme.first remaining with
      | None -> None
      | Some (grapheme, rest) ->
          let consumed = length remaining - length rest in
          state.current_pos <- state.current_pos + consumed;
          Some grapheme
    else
      None

  let size = fun { current_pos; source } -> length source - current_pos

  let clone = fun { source; current_pos } -> { source; current_pos }
end

let into_grapheme_mut_iter = fun source ->
  MutIterator.make (module GraphemeMutIter) { source; current_pos = 0 }

module GraphemeIter = struct
  type state = {
    source: string;
    current_pos: int;
  }

  type item = Unicode.Grapheme.t

  let next = fun ({ source; current_pos } as state) ->
    if current_pos < length source then
      let remaining = sub source current_pos (length source - current_pos) in
      match Unicode.Grapheme.first remaining with
      | None -> (None, state)
      | Some (grapheme, rest) ->
          let consumed = length remaining - length rest in
          (Some grapheme, { state with current_pos = current_pos + consumed })
    else
      (None, state)

  let size = fun { current_pos; source } -> length source - current_pos
end

let into_grapheme_iter = fun source ->
  Iterator.make (module GraphemeIter) { source; current_pos = 0 }

(* Text segmentation *)

let word_boundaries = fun s -> Unicode.Segmentation.find_word_boundaries s

let split_words = fun s ->
  let boundaries = word_boundaries s in
  let rec split = fun start ->
    function
    | [] ->
        if start < length s then
          [ sub s start (length s - start) ]
        else
          []
    | pos :: rest ->
        let word = trim (sub s start (pos - start)) in
        if word = "" then
          split pos rest
        else
          word :: split pos rest
  in
  split 0 boundaries

let line_breaks = fun s -> Unicode.Segmentation.find_line_breaks s

let wrap = fun ~width:_ s ->
  (* Simplified: split on whitespace *)
  split_on_char ' ' s |> List.filter (fun w -> w != "")

let wrap_words = fun ~width:target_width s ->
  let words = split_words s in
  let rec build_lines = fun current_line current_width ->
    function
    | [] ->
        if current_line = "" then
          []
        else
          [ trim current_line ]
    | word :: rest ->
        let word_width = width word in
        let space_width =
          if current_line = "" then
            0
          else
            1
        in
        if current_width + space_width + word_width <= target_width then
          let new_line =
            if current_line = "" then
              word
            else
              current_line ^ " " ^ word
          in
          build_lines new_line (current_width + space_width + word_width) rest
        else if current_line = "" then
          word :: build_lines "" 0 rest
        else
          (* Start new line *)
          trim current_line :: build_lines word word_width rest
  in
  build_lines "" 0 words

(** Check if a string contains a substring *)
let contains = fun haystack needle ->
  let needle_len = length needle in
  let haystack_len = length haystack in
  if needle_len = 0 then
    true
  else if needle_len > haystack_len then
    false
  else
    let rec check pos =
      if pos > haystack_len - needle_len then
        false
      else if sub haystack pos needle_len = needle then
        true
      else
        check (pos + 1)
    in
    check 0
