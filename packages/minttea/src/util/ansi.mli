(**
   ANSI escape sequence utilities.

   This module provides utilities for working with ANSI escape sequences,
   including measuring display width, stripping codes, and truncating text
   while preserving formatting.

   ## Example: Measuring Width

   ```ocaml
   open Std
   open Minttea

   let styled = "\027[1;31mHello\027[0m"  (* Bold red "Hello" *)
   let width = Ansi.width styled  (* Returns 5, not 18 *)
   ```

   ## Example: Truncating

   ```ocaml
   let long = "\027[1mVery long text here\027[0m"
   let short = Ansi.truncate ~width:10 ~ellipsis:"..." long
   (* Returns "\027[1mVery lo...\027[0m" - preserves bold *)
   ```
*)
val width: string -> int

(**
   `width str` returns the display width of `str`, ignoring ANSI escape codes.

   Only counts visible characters. Handles:
   - SGR sequences (colors, styles)
   - Cursor movement sequences
   - Other CSI sequences

   Does NOT handle:
   - Multi-column Unicode characters (counts as 1)
   - Zero-width characters (counts as 1)

   For basic ASCII and styled text, this is accurate.
*)
val strip: string -> string

(**
   `strip str` removes all ANSI escape sequences from `str`.

   Returns only the visible text content. Useful for:
   - Saving plain text
   - Comparing content
   - Length calculations when you need raw string length
*)
val truncate: width:int -> ?ellipsis:string -> string -> string

(**
   `truncate ~width ~ellipsis str` truncates `str` to fit in `width` columns.

   - Preserves ANSI formatting around truncated text
   - Adds `ellipsis` (default "…") if text was truncated
   - Ellipsis counts toward the width limit
   - Returns original string if it already fits

   Example:
   ```ocaml
   truncate ~width:8 "\027[31mVery long text\027[0m"
   (* Returns "\027[31mVery lo…\027[0m" *)
   ```
*)
val pad_right: width:int -> char -> string -> string

(**
   `pad_right ~width c str` pads `str` on the right with `c` to reach `width`.

   Measures display width correctly, accounting for ANSI codes.
   If `str` is already wider than `width`, returns it unchanged.
*)
val pad_left: width:int -> char -> string -> string

(** `pad_left ~width c str` pads `str` on the left with `c` to reach `width`. *)
val pad_center: width:int -> char -> string -> string

(** `pad_center ~width c str` centers `str` and pads with `c` to reach `width`. *)
val split_lines: string -> string list

(**
   `split_lines str` splits on newlines, preserving ANSI state across lines.

   Each line retains its formatting. If a style spans multiple lines,
   the style is closed and reopened appropriately.
*)
val word_wrap: width:int -> string -> string list

(**
   `word_wrap ~width str` wraps text to fit within `width` columns.

   - Breaks on word boundaries when possible
   - Preserves ANSI formatting codes
   - Handles multi-line input (splits on newlines first)
   - Breaks long words that exceed width
   - Returns list of wrapped lines

   Example:
   ```ocaml
   word_wrap ~width:10 "This is a very long line"
   (* Returns ["This is a"; "very long"; "line"] *)
   ```
*)
type ansi_state = {
  bold: bool;
  italic: bool;
  underline: bool;
  fg_color: string option;
  (* SGR color code *)
  bg_color: string option;
  (* SGR color code *)
}

(** Current ANSI formatting state *)
val parse_state: string -> ansi_state

(**
   `parse_state str` extracts the ANSI formatting state at the end of `str`.

   Useful for continuing formatting across line breaks or concatenations.
*)
val state_to_codes: ansi_state -> string

(**
   `state_to_codes state` converts formatting state back to ANSI codes.

   Generates the escape sequence needed to restore this state.
*)
