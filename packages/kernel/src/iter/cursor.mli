(** # Cursor - Immutable string cursor for parsing

    An immutable cursor that provides safe, functional string traversal. Each
    operation returns a new cursor, leaving the original unchanged.

    ## Core Operations

    - [peek] / [peek_n] - Look at characters without consuming
    - [advance] / [advance_by] - Move cursor forward
    - [take_while] / [skip_while] - Consume based on predicate
    - [remaining] - Get rest of string

    ## When to Use

    Use [Cursor] for backtracking parsers. For single-pass parsing, use
    [MutCursor]. *)

open Global0

type t

val create : string -> t
val source : t -> string
val position : t -> int
val length_remaining : t -> int
val is_eof : t -> bool
val peek : t -> char option
val peek_n : t -> int -> char option
val advance : t -> t option
val advance_by : t -> int -> t option
val take_while : t -> (char -> bool) -> string * t
val skip_while : t -> (char -> bool) -> t

val take_until : t -> (char -> bool) -> (string * t) option
(** Takes characters until predicate returns true. Returns (taken_string,
    cursor_at_matching_char) or None if predicate never matches.

    ## Examples

    ```ocaml (* Take until space *) match Cursor.take_until cursor (fun c -> c =
    ' ') with | Some (token, cursor) -> (* got "GET", cursor at space *) | None
    -> (* no space found, need more data *)

    (* Take until CRLF *) match Cursor.take_until cursor (fun c -> c = '\r')
    with | Some (line, cursor) -> (* Skip the \r\n *) let cursor =
    Cursor.advance_by cursor 2 |> Option.unwrap in process line cursor | None ->
    Need_more ``` *)

val take_n : t -> int -> (string * t) option
(** Takes exactly n characters. Returns None if fewer than n remain. *)

val remaining : t -> string
