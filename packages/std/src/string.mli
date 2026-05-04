(**
   UTF-8 string manipulation.

   This module extends OCaml's standard String module with UTF-8 aware
   iteration support. Strings in OCaml are sequences of bytes, and this module
   provides safe UTF-8 character iteration on top of that.

   ## Examples

   Basic string operations:

   ```ocaml open Std

   let text = "Hello, 世界!" in

   (* Standard operations *) let upper = String.uppercase_ascii text in let
   lower = String.lowercase_ascii text in let trimmed = String.trim " spaces "
   in

   (* UTF-8 iteration *) String.into_iter text |> Iterator.for_each (fun rune
   -> Printf.printf "Char: %s\n" (Unicode.Rune.to_string rune)) ```

   ## UTF-8 Support

   While OCaml strings are byte sequences, this module provides:
   - UTF-8 character iteration via [`into_iter`] and [`into_mut_iter`]
   - All standard String module functions for byte-level operations

   ## Common Patterns

   ```ocaml (* Check if string contains substring *) let contains_word text
   word = String.contains text word

   (* Split string into lines *) let lines = String.split_on_char '\n' text

   (* Join strings *) let csv = String.concat "," ["a"; "b"; "c"] ```
*)
open Iter

type t = string
type utf_decode = Kernel.Unicode.Rune.utf_decode

val empty: t

val is_empty: t -> bool

val length: t -> int

val get: t -> at:int -> char option

val get_unchecked: t -> at:int -> char

(** Use `unsafe_get value index` as the conventional alias for `get_unchecked value ~at:index`. *)
val unsafe_get: t -> int -> char

val sub: t -> offset:int -> len:int -> t

(**
   Use `init length builder` to construct a fresh string by calling `builder` for each index from
   left to right.
*)
val init: len:int -> fn:(int -> char) -> t

(** Use `make length char` to fill a fresh string with repeated copies of `char`. *)
val make: len:int -> char:char -> t

val from_char: char -> t

(** Use `append left right` to concatenate two strings into a fresh result. *)
val append: t -> t -> t

(** Use `concat separator values` to join `values` with `separator` into a fresh result. *)
val concat: t -> t list -> t

val contains: t -> t -> bool

val starts_with: prefix:t -> t -> bool

val ends_with: suffix:t -> t -> bool

val equal: t -> t -> bool

val compare: t -> t -> Order.t

val index_of: t -> char:char -> int option

val last_index: t -> char -> int option

val trim: t -> t

val split: by:t -> t -> t list

(**
   Use `split_on_char separator value` as the conventional alias for splitting on a
   one-character separator.
*)
val split_on_char: char -> t -> t list

val lowercase_ascii: t -> t

val capitalize_ascii: t -> t

val uppercase_ascii: t -> t

val map: fn:(char -> char) -> t -> t

val for_each: fn:(char -> unit) -> t -> unit

(** Use `iter fn value` as the conventional alias for `for_each ~fn value`. *)
val iter: (char -> unit) -> t -> unit

val exists: fn:(char -> bool) -> t -> bool

val for_all: fn:(char -> bool) -> t -> bool

val fold_left: fn:('acc -> char -> 'acc) -> init:'acc -> t -> 'acc

val escaped: t -> t

val get_utf_8_rune: t -> at:int -> utf_decode option

(** Use `from_bytes value` to copy `value` into a fresh immutable string. *)
val from_bytes: bytes -> t

(** Use `unsafe_from_bytes value` is basically a cast *)
val unsafe_from_bytes: bytes -> t

(** Use `to_bytes value` to copy `value` into fresh mutable bytes. *)
val to_bytes: t -> bytes

(**
   Creates a mutable iterator over UTF-8 characters.

   Iterates over Unicode characters (not bytes) in the string. Invalid UTF-8
   sequences are replaced with the replacement character.

   ## Examples

   ```ocaml let text = "Hello, 世界!" in let iter = String.into_mut_iter text in

   (* Count characters (not bytes) *) let char_count = MutIterator.count iter
   in println "Characters: %d" char_count;

   (* Process each character *) String.into_mut_iter text |>
   MutIterator.for_each (fun rune -> let code = Unicode.Rune.to_int rune in
   Printf.printf "U+%04X " code) ```

   ## Performance

   UTF-8 decoding has some overhead. For byte-level operations, use standard
   String functions instead.
*)
val into_mut_iter: string -> Unicode.Rune.t MutIterator.t

(**
   Creates an immutable iterator over UTF-8 characters.

   Similar to [`into_mut_iter`] but returns an immutable iterator suitable for
   functional transformations.

   ## Examples

   ```ocaml let text = "Café ☕" in

   (* Filter non-ASCII characters *) let ascii_only = String.into_iter text |>
   Iterator.filter (fun rune -> Unicode.Rune.to_int rune < 128) |> Iterator.to_list |>
   List.map Kernel.Unicode.Rune.to_char |> String.from_list in (* ascii_only = "Caf " *)

   (* Count emoji *) let emoji_count = String.into_iter text |> Iterator.filter
   (fun rune -> let code = Unicode.Rune.to_int rune in code >= 0x1F600 && code <= 0x1F64F)
   |> Iterator.count ```

   ## UTF-8 Handling

   Invalid UTF-8 sequences are replaced with U+FFFD (�). For strict UTF-8
   validation, check bytes before iteration.
*)
val into_iter: string -> Unicode.Rune.t Iterator.t

(**
   Calculate display width for monospace fonts/terminals.

   Accounts for:
   - Wide characters (CJK): width 2
   - Emoji: width 2
   - Combining marks: width 0
   - Control characters: width 0
   - Regular characters: width 1

   ## Examples

   ```ocaml
   String.width "hello"      (* = 5 *)
   String.width "你好"        (* = 4, each CJK char is width 2 *)
   String.width "Hello 👋"   (* = 8: 6 for "Hello " + 2 for emoji *)
   ```

   This is essential for proper text alignment in terminals.
*)
val width: string -> int

(**
   Count Unicode code points (runes) in the string.

   ## Examples

   ```ocaml
   String.rune_count "hello"       (* = 5 *)
   String.rune_count "café"        (* = 4 *)
   String.rune_count "👋"          (* = 1 *)
   String.rune_count "👨‍👩‍👧‍👦"     (* = 7: base + joiners + others *)
   ```

   Note: This counts code points, not user-perceived characters.
   Use `grapheme_count` for user-perceived character count.
*)
val rune_count: string -> int

(**
   Count user-perceived characters (grapheme clusters).

   ## Examples

   ```ocaml
   String.grapheme_count "hello"       (* = 5 *)
   String.grapheme_count "café"        (* = 4 *)
   String.grapheme_count "👋"          (* = 1 *)
   String.grapheme_count "👨‍👩‍👧‍👦"     (* = 1: family emoji is one grapheme *)
   String.grapheme_count "🏳️‍🌈"       (* = 1: rainbow flag is one grapheme *)
   ```

   This gives the count users would expect when counting "characters".
*)
val grapheme_count: string -> int

(**
   Truncate string to fit within display width.

   ## Parameters

   - `width`: Maximum display width
   - `tail`: String to append if truncated (default: "...")

   ## Examples

   ```ocaml
   String.truncate_width ~width:10 "Hello World"
   (* = "Hello W..." *)

   String.truncate_width ~width:10 ~tail:"…" "Hello 世界"
   (* = "Hello 世…" - CJK chars are width 2 *)

   String.truncate_width ~width:10 "Short"
   (* = "Short" - no truncation needed *)
   ```

   Useful for fitting text in fixed-width terminal columns.
*)
val truncate_width: width:int -> ?tail:string -> string -> string

(**
   Pad string on the left to reach display width.

   ## Examples

   ```ocaml
   String.pad_left ~width:10 ' ' "Hello"
   (* = "     Hello" *)

   String.pad_left ~width:10 '0' "42"
   (* = "00000000042" *)
   ```

   Uses display width, so handles wide characters correctly.
*)
val pad_left: width:int -> char -> string -> string

(**
   Pad string on the right to reach display width.

   ## Examples

   ```ocaml
   String.pad_right ~width:10 ' ' "Hello"
   (* = "Hello     " *)
   ```

   Uses display width, so handles wide characters correctly.
*)
val pad_right: width:int -> char -> string -> string

(**
   Pad string on both sides to center within display width.

   ## Examples

   ```ocaml
   String.pad_center ~width:10 ' ' "Hi"
   (* = "    Hi    " *)
   ```

   If padding is uneven, adds extra space on the right.
*)
val pad_center: width:int -> char -> string -> string

(**
   Creates an iterator over grapheme clusters.

   ## Examples

   ```ocaml
   String.into_grapheme_iter "Hello 👨‍👩‍👧‍👦"
   |> Iterator.count  (* = 7: 6 regular chars + 1 family emoji *)
   ```

   Iterates over user-perceived characters, not code points.
*)
val into_grapheme_iter: string -> Unicode.Grapheme.t Iterator.t

(**
   Creates a mutable iterator over grapheme clusters.

   Similar to `into_grapheme_iter` but returns a mutable iterator.
*)
val into_grapheme_mut_iter: string -> Unicode.Grapheme.t MutIterator.t

(**
   Find byte positions of word boundaries.

   ## Examples

   ```ocaml
   String.word_boundaries "Hello world"
   (* = [5; 11] - after "Hello" and "world" *)
   ```

   Uses simplified word boundary detection.
*)
val word_boundaries: string -> int list

(**
   Split string into words.

   ## Examples

   ```ocaml
   String.split_words "Hello world"
   (* = ["Hello"; "world"] *)
   ```

   Uses simplified word boundary detection.
*)
val split_words: string -> string list

(**
   Find line break opportunities.

   Returns list of (position, break_type) where:
   - `Must_break`: Line must break (newline)
   - `Can_break`: Line may break (word boundary)
   - `Dont_break`: Line must not break

   ## Examples

   ```ocaml
   String.line_breaks "Hello\nworld"
   (* = [(5, Must_break); (11, Dont_break)] *)
   ```

   Useful for text wrapping and line breaking.
*)
val line_breaks: string -> (int * Unicode.line_break) list

(**
   Wrap text to fit within display width.

   ## Examples

   ```ocaml
   String.wrap ~width:10 "Hello beautiful world"
   (* = ["Hello"; "beautiful"; "world"] *)
   ```

   Breaks at word boundaries when possible.
*)
val wrap: width:int -> string -> string list

(**
   Wrap text at word boundaries to fit within display width.

   Similar to `wrap` but ensures words aren't broken.
*)
val wrap_words: width:int -> string -> string list

(**
   [contains haystack needle] returns [true] if [haystack] contains [needle] as a substring.

   Example:
   ```ocaml
   String.contains "hello world" "world"  (* = true *)
   String.contains "hello world" "foo"    (* = false *)
   String.contains "test" ""              (* = true *)
   ```

   Note: Empty string is considered to be contained in any string.
*)
val contains: string -> string -> bool

(**
   [to_reader ?chunk_size value] creates an [IO.Reader] over [value].

   The reader can be forced to yield smaller chunks, which is useful for
   testing incremental decoders and other streaming APIs.

   @raise Invalid_argument if [chunk_size <= 0].
*)
val to_reader: ?chunk_size:int -> string -> IO.Reader.t

module Syntax: sig
  val get: string -> int -> char

  val set: string -> int -> char -> unit
end
