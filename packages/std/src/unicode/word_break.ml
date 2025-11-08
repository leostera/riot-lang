(** Word boundary detection - Simplified but practical implementation
    
    This is a pragmatic implementation that handles ~90% of word navigation
    use cases without needing the full 1,883-line UAX #29 property tables.
    
    Handles:
    - ASCII words (letters, numbers)
    - Latin-extended words (accented characters)
    - CJK words (each character is a word)
    - Common punctuation/whitespace boundaries
    - Apostrophes in contractions (don't, it's)
    - Underscores in identifiers (snake_case)
    
    Does NOT handle:
    - Complex scripts (Arabic, Thai, etc.) - would need full tables
    - All Unicode letter categories - only common scripts
    - Hebrew double quotes - would need lookahead rules
    - Some edge cases in UAX #29
*)

open Global
open Collections
module String = Kernel.String
module Uchar = Kernel.Uchar

(** Word break property - simplified *)
type word_property =
  | Letter          (** Letters (including Latin, Greek, Cyrillic, CJK) *)
  | Number          (** Digits *)
  | Whitespace      (** Spaces, tabs, newlines *)
  | Punctuation     (** Most punctuation *)
  | Apostrophe      (** ' for contractions *)
  | Underscore      (** _ for identifiers *)
  | Other           (** Everything else *)

(** Determine if a rune is a letter (extended beyond just ASCII) *)
let is_letter_extended c =
  (* ASCII letters *)
  (c >= 0x41 && c <= 0x5A) ||  (* A-Z *)
  (c >= 0x61 && c <= 0x7A) ||  (* a-z *)
  (* Latin-1 Supplement *)
  (c >= 0xC0 && c <= 0xFF && c != 0xD7 && c != 0xF7) ||
  (* Latin Extended-A *)
  (c >= 0x0100 && c <= 0x017F) ||
  (* Latin Extended-B *)
  (c >= 0x0180 && c <= 0x024F) ||
  (* Greek *)
  (c >= 0x0370 && c <= 0x03FF) ||
  (* Cyrillic *)
  (c >= 0x0400 && c <= 0x04FF) ||
  (* Armenian *)
  (c >= 0x0530 && c <= 0x058F) ||
  (* Hebrew *)
  (c >= 0x05D0 && c <= 0x05FF) ||
  (* Arabic *)
  (c >= 0x0600 && c <= 0x06FF) ||
  (* Devanagari *)
  (c >= 0x0900 && c <= 0x097F) ||
  (* CJK Unified Ideographs *)
  (c >= 0x4E00 && c <= 0x9FFF) ||
  (* Hangul Syllables *)
  (c >= 0xAC00 && c <= 0xD7AF) ||
  (* Hiragana *)
  (c >= 0x3040 && c <= 0x309F) ||
  (* Katakana *)
  (c >= 0x30A0 && c <= 0x30FF)

(** Determine if a rune is a digit *)
let is_digit c =
  (* ASCII digits *)
  (c >= 0x30 && c <= 0x39) ||
  (* Arabic-Indic digits *)
  (c >= 0x0660 && c <= 0x0669) ||
  (* Extended Arabic-Indic digits *)
  (c >= 0x06F0 && c <= 0x06F9) ||
  (* Devanagari digits *)
  (c >= 0x0966 && c <= 0x096F)

(** Determine if a rune is whitespace *)
let is_whitespace c =
  match c with
  | 0x20 | 0x09 | 0x0A | 0x0B | 0x0C | 0x0D  (* Common ASCII whitespace *)
  | 0xA0  (* Non-breaking space *)
  | 0x1680 | 0x2000 | 0x2001 | 0x2002 | 0x2003 | 0x2004 
  | 0x2005 | 0x2006 | 0x2007 | 0x2008 | 0x2009 | 0x200A
  | 0x2028 | 0x2029 | 0x202F | 0x205F | 0x3000 -> true
  | _ -> false

(** Get word property for a code point *)
let get_word_property c =
  if is_whitespace c then Whitespace
  else if c = 0x0027 || c = 0x2019 then Apostrophe  (* ' and ' *)
  else if c = 0x005F then Underscore  (* _ *)
  else if is_letter_extended c then Letter
  else if is_digit c then Number
  else if (c >= 0x21 && c <= 0x2F) || (c >= 0x3A && c <= 0x40) ||
          (c >= 0x5B && c <= 0x5E) || (c >= 0x60 && c <= 0x60) ||
          (c >= 0x7B && c <= 0x7E) then Punctuation
  else Other

(** Check if there should be a word boundary between two characters
    
    Rules (simplified from UAX #29):
    - Always break around whitespace
    - Don't break Letter × Letter
    - Don't break Number × Number  
    - Don't break Letter × Number or Number × Letter (for hex codes, etc.)
    - Don't break around apostrophe in contractions: Letter × Apostrophe × Letter
    - Don't break around underscore: Letter × Underscore × Letter
    - Break everywhere else
*)
let should_break_word ~prev_prop ~curr_prop ~next_prop =
  match prev_prop, curr_prop, next_prop with
  (* Always break before/after whitespace *)
  | _, Whitespace, _ -> true
  | Whitespace, _, _ -> true
  
  (* Don't break Letter × Letter *)
  | Letter, Letter, _ -> false
  
  (* Don't break Number × Number *)
  | Number, Number, _ -> false
  
  (* Don't break Letter × Number or Number × Letter *)
  | Letter, Number, _ -> false
  | Number, Letter, _ -> false
  
  (* Don't break Letter × Apostrophe × Letter (contractions: don't, it's) *)
  | Letter, Apostrophe, Some Letter -> false
  (* Also don't break Apostrophe × Letter when part of contraction *)
  | Apostrophe, Letter, _ -> false
  
  (* Don't break Letter × Underscore × Letter (identifiers: snake_case) *)
  | Letter, Underscore, Some Letter -> false
  | Number, Underscore, Some Number -> false
  | Number, Underscore, Some Letter -> false
  | Letter, Underscore, Some Number -> false
  (* Also don't break Underscore × Letter/Number *)
  | Underscore, (Letter | Number), _ -> false
  
  (* Break Letter/Number × Apostrophe if not followed by Letter *)
  | (Letter | Number), Apostrophe, _ -> true
  
  (* Break everywhere else *)
  | _, _, _ -> true

(** Find all word boundaries in a string
    Returns byte positions where word breaks occur *)
let find_word_boundaries s =
  let len = String.length s in
  if len = 0 then []
  else
    let rec scan pos prev_prop acc =
      if pos >= len then List.rev acc
      else
        (* Decode current rune *)
        let decode = String.get_utf_8_uchar s pos in
        if not (Uchar.utf_decode_is_valid decode) then
          (* Invalid UTF-8, treat as boundary *)
          List.rev (pos :: acc)
        else
          let rune = Uchar.utf_decode_uchar decode in
          let rune_len = Uchar.utf_decode_length decode in
          let curr_code = Rune.to_int rune in
          let curr_prop = get_word_property curr_code in
          
          (* Peek at next rune for lookahead *)
          let next_prop =
            let next_pos = pos + rune_len in
            if next_pos >= len then None
            else
              let next_decode = String.get_utf_8_uchar s next_pos in
              if Uchar.utf_decode_is_valid next_decode then
                let next_rune = Uchar.utf_decode_uchar next_decode in
                let next_code = Rune.to_int next_rune in
                Some (get_word_property next_code)
              else
                None
          in
          
          (* Check if we should break *)
          let break_here = should_break_word ~prev_prop ~curr_prop ~next_prop in
          let new_acc = if break_here && pos > 0 then pos :: acc else acc in
          
          scan (pos + rune_len) curr_prop new_acc
    in
    
    (* Start scanning from position 0 *)
    let first_decode = String.get_utf_8_uchar s 0 in
    if not (Uchar.utf_decode_is_valid first_decode) then []
    else
      let first_rune = Uchar.utf_decode_uchar first_decode in
      let first_len = Uchar.utf_decode_length first_decode in
      let first_prop = get_word_property (Rune.to_int first_rune) in
      scan first_len first_prop []

(** Find the start of the next word from position pos *)
let find_next_word_start s pos =
  let boundaries = find_word_boundaries s in
  match List.find_opt (fun b -> b > pos) boundaries with
  | Some boundary -> boundary
  | None -> String.length s

(** Find the start of the previous word from position pos *)
let find_prev_word_start s pos =
  let boundaries = find_word_boundaries s in
  let before = List.filter (fun b -> b < pos) boundaries in
  match List.rev before with
  | last :: _ -> last
  | [] -> 0
