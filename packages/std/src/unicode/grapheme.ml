(** Grapheme clusters - user-perceived characters *)
open Prelude
module String = Kernel.String
module List = Collections.List
module Scalar = Kernel.Unicode.Rune
module Rune = Rune

type t = Rune.t list

(**
   Extract the first grapheme cluster from a string

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
    match String.get_utf_8_rune s ~at:0 with
    | None -> None
    | Some decode ->
        if not (Scalar.utf_decode_is_valid decode) then
          None
        else
          let first_rune = Scalar.utf_decode_rune decode in
          let first_len = Scalar.utf_decode_length decode in
          (* Build the grapheme cluster by consuming runes that shouldn't break *)
          let rec consume_cluster pos cluster prev_prop has_zwj =
            if pos >= String.length s then
              (List.reverse cluster, "")
            else
              match String.get_utf_8_rune s ~at:pos with
              | None -> (List.reverse cluster, String.sub s ~offset:pos ~len:(String.length s - pos))
              | Some decode ->
                  if not (Scalar.utf_decode_is_valid decode) then
                    (List.reverse cluster, String.sub s ~offset:pos ~len:(String.length s - pos))
                  else
                    let curr_rune = Scalar.utf_decode_rune decode in
                    let curr_len = Scalar.utf_decode_length decode in
                    let curr_code = Rune.to_int curr_rune in
                    let curr_prop = Grapheme_break.get_break_property curr_code in
                    (* Check if we should break *)
                    if Grapheme_break.should_break ~prev_prop ~curr_prop ~has_zwj then
                      let rest = String.sub s ~offset:pos ~len:(String.length s - pos) in
                      (List.reverse cluster, rest)
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
      let base_width =
        match List.head runes with
        | Some rune -> Rune.width rune
        | None -> 0
      in
      (* For graphemes with extending characters, use base width *)
      (* This handles combining marks and emoji modifiers correctly *)
      base_width

let to_string = fun grapheme ->
  String.concat "" (List.map grapheme ~fn:Rune.to_string)
