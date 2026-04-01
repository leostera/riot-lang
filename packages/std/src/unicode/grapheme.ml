(** Grapheme clusters - user-perceived characters *)
open Global
module String = Kernel.String
module List = Kernel.Collections.List
module Uchar = Kernel.Uchar

type t = Rune.t list
(** Extract the first grapheme cluster from a string
    
    This implements UAX #29 grapheme cluster boundary detection
    using the simplified rules from Grapheme_break module.
    
    Handles:
    - Combining marks (e.g., "é" = 'e' + combining acute)
    - Emoji ZWJ sequences (e.g., family emoji)
    - Regional indicator pairs (flags)
    - Hangul syllables
*)
let first = fun s ->
  if s = "" then
    None
  else
    (* Decode first rune *)
    let decode = String.get_utf_8_uchar s 0 in
    if not (Uchar.utf_decode_is_valid decode) then
      None
    else
      let first_rune = Uchar.utf_decode_uchar decode in
      let first_len = Uchar.utf_decode_length decode in
      (* Build the grapheme cluster by consuming runes that shouldn't break *)
      let rec consume_cluster pos cluster prev_prop has_zwj =
        if pos >= String.length s then
          (List.rev cluster, "")
        else
          let decode = String.get_utf_8_uchar s pos in
          if not (Uchar.utf_decode_is_valid decode) then
            (List.rev cluster, String.sub s pos (String.length s - pos))
          else
            let curr_rune = Uchar.utf_decode_uchar decode in
            let curr_len = Uchar.utf_decode_length decode in
            let curr_code = Rune.to_int curr_rune in
            let curr_prop = Grapheme_break.get_break_property curr_code in
            (* Check if we should break *)
            if Grapheme_break.should_break ~prev_prop ~curr_prop ~has_zwj then
              let rest = String.sub s pos (String.length s - pos) in
              (List.rev cluster, rest)
            else
              (* Don't break - add to cluster and continue *)
              let new_has_zwj = has_zwj || (curr_code = 0x200d) in
              consume_cluster (pos + curr_len) (curr_rune :: cluster) curr_prop new_has_zwj
      in
      let first_code = Rune.to_int first_rune in
      let first_prop = Grapheme_break.get_break_property first_code in
      let (cluster, rest) = consume_cluster first_len [ first_rune ] first_prop false in
      Some (cluster, rest)

let width = fun grapheme ->
  (* Width of a grapheme cluster
     
     For most graphemes, this is the maximum width of any rune in the cluster.
     However, for proper handling:
     - ZWJ sequences (emoji): use the base character's width (typically 2)
     - Combining marks: width 0, so they don't affect the base
     - Regional indicators: count as a pair (width 2 for the pair)
  *)
  match grapheme with
  | [] -> 0
  | runes ->
      (* Get the width of the first (base) character *)
      let base_width = Rune.width (List.hd runes) in
      (* For graphemes with extending characters, use base width *)
      (* This handles combining marks and emoji modifiers correctly *)
      base_width

let to_string = fun grapheme ->
  String.concat "" (List.map Rune.to_string grapheme)
