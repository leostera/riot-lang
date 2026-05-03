(**
   Unicode text processing support.

   This module provides comprehensive Unicode support including:
   - Rune (code point) operations
   - Grapheme cluster handling
   - Display width calculation for terminals
   - UTF-8 encoding/decoding
   - Text segmentation (words, sentences, line breaks)

   {1 Quick Start}

   {[
     open Std

     (* Count user-perceived characters *)
     let count = String.grapheme_count "Hello 👨‍👩‍👧‍👦!"  (* = 8 *)

     (* Calculate display width for terminal *)
     let width = String.width "你好世界"  (* = 8, each CJK char is width 2 *)

     (* Truncate to display width *)
     let truncated = String.truncate_width ~width:10 "Hello 世界!"

     (* Iterate over runes *)
     String.into_iter "Hello"
     |> Iterator.for_each (fun rune ->
         Printf.printf "U+%04X\n" (Unicode.Rune.to_int rune))
   ]}

   {1 Key Concepts}

   - {b Rune}: A Unicode code point (U+0000 to U+10FFFF)
   - {b Grapheme cluster}: What users perceive as a single character
     (can be multiple runes, e.g., emoji with modifiers)
   - {b Display width}: Number of terminal cells a string occupies
     (ASCII=1, CJK=2, emoji=2, combining=0)
*)
module Rune: sig
  type t = Kernel.Unicode.Rune.t

  (**
     A Unicode code point (scalar value).

     Valid range: U+0000 to U+10FFFF, excluding surrogates.
  *)
  (** {2 Constants} *)

  val max: t

  (** [max] is U+10FFFF, the maximum valid Unicode code point. *)
  val replacement: t

  (**
     [replacement] is U+FFFD, the Unicode replacement character.
     Used to represent invalid or unrepresentable characters.
  *)
  val max_ascii: t

  (** [max_ascii] is U+007F, the maximum ASCII character. *)
  val max_latin1: t

  (** [max_latin1] is U+00FF, the maximum Latin-1 character. *)
  (** {2 Conversion} *)

  val from_int: int -> t option

  (**
     [from_int n] converts an integer to a rune.
     Returns [None] if n is not a valid Unicode code point.
  *)
  val to_int: t -> int

  (** [to_int r] returns the integer value of rune [r]. *)
  val from_char: char -> t

  (** [from_char c] converts an 8-bit character to a rune. *)
  val to_char: t -> char

  (** [to_char r] converts an ASCII or Latin-1 rune back to a char. *)
  val to_string: t -> string

  (** [to_string r] encodes rune [r] as a UTF-8 string. *)
  val from_int_unchecked: int -> t

  (**
     [from_int_unchecked n] converts an integer to a rune without validation.
     {b Warning}: Only use if you know [n] is a valid code point.
  *)
  (** {2 Character Classification} *)

  val is_letter: t -> bool

  (** [is_letter r] tests if [r] is a letter (category L). *)
  val is_digit: t -> bool

  (** [is_digit r] tests if [r] is a decimal digit (category Nd). *)
  val is_space: t -> bool

  (**
     [is_space r] tests if [r] is a whitespace character.
     Includes: space, tab, newline, and Unicode spaces.
  *)
  val is_control: t -> bool

  (** [is_control r] tests if [r] is a control character. *)
  val is_print: t -> bool

  (** [is_print r] tests if [r] is printable (not a control character). *)
  val is_graphic: t -> bool

  (**
     [is_graphic r] tests if [r] is a graphic character.
     Includes letters, marks, numbers, punctuation, symbols, and spaces.
  *)
  val is_mark: t -> bool

  (** [is_mark r] tests if [r] is a combining mark (category M). *)
  val is_number: t -> bool

  (** [is_number r] tests if [r] is a number (category N). *)
  val is_punct: t -> bool

  (** [is_punct r] tests if [r] is punctuation (category P). *)
  val is_symbol: t -> bool

  (** [is_symbol r] tests if [r] is a symbol (category S). *)
  (** {2 Case Operations} *)

  val is_upper: t -> bool

  (** [is_upper r] tests if [r] is an uppercase letter. *)
  val is_lower: t -> bool

  (** [is_lower r] tests if [r] is a lowercase letter. *)
  val is_title: t -> bool

  (** [is_title r] tests if [r] is a titlecase letter. *)
  val to_upper: t -> t

  (**
     [to_upper r] converts [r] to uppercase.
     Returns [r] unchanged if no uppercase mapping exists.
  *)
  val to_lower: t -> t

  (**
     [to_lower r] converts [r] to lowercase.
     Returns [r] unchanged if no lowercase mapping exists.
  *)
  val to_title: t -> t

  (**
     [to_title r] converts [r] to titlecase.
     Returns [r] unchanged if no titlecase mapping exists.
  *)
  (** {2 Display Width} *)

  val width: t -> int

  (**
     [width r] returns the display width of [r] in a monospace terminal.

     Returns:
     - 0 for control characters, combining marks, zero-width joiners
     - 1 for most characters
     - 2 for wide characters (CJK, emoji, etc.)

     This follows EastAsianWidth properties and grapheme cluster rules.
  *)
  (** {2 East Asian Width Properties} *)

  val is_wide: t -> bool

  (** [is_wide r] tests if [r] has East Asian Width property "Wide" (W). *)
  val is_fullwidth: t -> bool

  (** [is_fullwidth r] tests if [r] has East Asian Width property "Fullwidth" (F). *)
  val is_ambiguous: t -> bool

  (**
     [is_ambiguous r] tests if [r] has East Asian Width property "Ambiguous" (A).
     These characters have width 1 or 2 depending on locale.
  *)
end

module Grapheme: sig
  type t = Rune.t list

  (**
     A grapheme cluster is a sequence of one or more runes that form
     a single user-perceived character.

     Examples:
     - ['e'; '́'] (e + combining acute accent) = "é"
     - Family emoji = base + zero-width joiners + other emoji
     - Regional indicator pairs = flags
  *)
  val first: string -> (t * string) option

  (**
     [first s] returns the first grapheme cluster in [s] and the remaining string.
     Returns [None] if [s] is empty.
  *)
  val width: t -> int

  (**
     [width g] returns the display width of grapheme cluster [g].

     This accounts for combining characters, emoji, and other special cases.
  *)
  val to_string: t -> string

  (** [to_string g] encodes grapheme cluster [g] as a UTF-8 string. *)
end

module Utf8: sig
  val decode_rune: string -> int -> (Rune.t * int) option

  (**
     [decode_rune s pos] decodes the UTF-8 rune starting at byte position [pos].

     Returns [Some (rune, next_pos)] where [next_pos] is the position after the rune.
     Returns [None] if [pos] is out of bounds or invalid UTF-8.
  *)
  val encode_rune: Rune.t -> string

  (** [encode_rune r] encodes rune [r] as a UTF-8 string (1-4 bytes). *)
  val is_valid: string -> bool

  (** [is_valid s] tests if [s] is valid UTF-8. *)
  val is_continuation: char -> bool

  (** [is_continuation c] tests if byte [c] is a UTF-8 continuation byte (10xxxxxx). *)
  val rune_length: char -> int

  (**
     [rune_length c] returns the expected length of a UTF-8 sequence
     starting with byte [c].

     Returns 1-4 for valid start bytes, 0 for continuation bytes or invalid bytes.
  *)
end

module Utf16: sig
  type position = { line: int; character: int }

  (** A zero-based line and UTF-16 code-unit offset within that line. *)
  val code_units_of_rune: Rune.t -> int

  (**
     [code_units_of_rune rune] returns how many UTF-16 code units [rune] occupies.

     Most runes occupy 1 code unit; supplementary-plane runes occupy 2.
  *)
  val position_of_offset: string -> offset:int -> position

  (**
     [position_of_offset text ~offset] converts a UTF-8 byte offset into a
     zero-based line and UTF-16 character position.

     Offsets are clamped into the document bounds. Newlines reset the UTF-16
     character count, and both [\n] and [\r\n] are treated as line breaks.
  *)
  val offset_of_position: string -> position -> (int, string) Result.t

  (**
     [offset_of_position text position] converts a zero-based UTF-16 position
     into a UTF-8 byte offset.

     Returns [Error _] when the line is out of bounds, the character extends
     beyond the end of the line, or the position would split a surrogate pair.
  *)
end

type line_break =
  | Must_break
  (** Line must break here (e.g., newline) *)
  | Can_break
  (** Line may break here (word boundary) *)
  | Dont_break

(** Line must not break here *)

(** Line breaking opportunities per Unicode UAX #14. *)
module Segmentation: sig
  val find_word_boundaries: string -> int list

  (**
     [find_word_boundaries s] returns byte positions of word boundaries in [s].

     Follows simplified UAX #29 word segmentation rules.
  *)
  val find_next_word_start: string -> int -> int

  (**
     [find_next_word_start s pos] returns the byte position of the next word
     boundary after [pos].

     Useful for Ctrl+Right arrow navigation. Returns string length if no more words.
  *)
  val find_prev_word_start: string -> int -> int

  (**
     [find_prev_word_start s pos] returns the byte position of the previous word
     boundary before [pos].

     Useful for Ctrl+Left arrow navigation. Returns 0 if at beginning.
  *)
  val find_sentence_boundaries: string -> int list

  (**
     [find_sentence_boundaries s] returns byte positions of sentence boundaries in [s].

     Follows Unicode UAX #29 sentence segmentation rules.
  *)
  val find_line_breaks: string -> (int * line_break) list

  (**
     [find_line_breaks s] returns positions and types of line break opportunities.

     Follows simplified UAX #14 line breaking algorithm.
  *)
  val wrap_lines: width:int -> string -> string list

  (**
     [wrap_lines ~width s] wraps text to fit within the given display width.

     Returns a list of lines, each fitting within [width] cells.
     Breaks at appropriate line break opportunities (spaces, punctuation, CJK boundaries).
     Handles mandatory breaks (newlines) and forced breaks when no opportunities exist.
  *)
end

module Config: sig
  val set_east_asian_width: bool -> unit

  (**
     [set_east_asian_width enabled] configures treatment of ambiguous width characters.

     When [enabled=true] (for CJK locales), ambiguous characters have width 2.
     When [enabled=false] (default, for Western locales), they have width 1.
  *)
  val get_east_asian_width: unit -> bool

  (** [get_east_asian_width ()] returns the current East Asian width setting. *)
end
