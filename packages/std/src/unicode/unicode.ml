(** Unicode - Unicode text processing support
    
    This module provides comprehensive Unicode support including:
    - Rune (code point) operations  
    - Grapheme cluster handling with proper UAX #29 support
    - Display width calculation for terminals using complete East Asian Width tables
    - UTF-8 encoding/decoding
    - Text segmentation (words, sentences, line breaks)
*)
(** {1 Submodules} *)

(** Width tables for character display width calculation *)
module Width_tables = Width_tables

(** Grapheme break properties and rules *)
module Grapheme_break = Grapheme_break

(** Word break properties and detection *)
module Word_break = Word_break

(** Line break properties and text wrapping *)
module Line_break = Line_break

(** Configuration for Unicode processing *)
module Config = Unicode_config

(** Rune - Unicode code points *)
module Rune = Rune

(** UTF-8 encoding and decoding *)
module Utf8 = Utf8

(** UTF-16 position and offset conversion helpers *)
module Utf16 = Utf16

(** Grapheme clusters - user-perceived characters *)
module Grapheme = Grapheme

(** Text segmentation *)
module Segmentation = Segmentation

(** {1 Type exports} *)

(** Line break type for text segmentation *)
type line_break = Segmentation.line_break =
  | Must_break
  (** Line must break here (e.g., newline) *)
  | Can_break
  (** Line may break here (word boundary) *)
  | Dont_break

(** Line must not break here *)
