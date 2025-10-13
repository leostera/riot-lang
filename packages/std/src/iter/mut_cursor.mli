(** # MutCursor - Mutable string cursor for parsing

    A mutable cursor that provides efficient string traversal for single-pass
    parsing. Operations mutate the cursor in place.

    ## Core Operations

    - [peek] / [peek_n] - Look at characters without advancing
    - [advance] / [advance_by] - Move cursor forward (mutating)
    - [take_while] / [skip_while] - Consume based on predicate (mutating)
    - [remaining] - Get rest of string

    ## When to Use

    Use [MutCursor] for efficient single-pass parsing. For backtracking, use
    [Cursor]. *)

type t

val create : string -> t
val source : t -> string
val position : t -> int
val length_remaining : t -> int
val is_eof : t -> bool
val peek : t -> char option
val peek_n : t -> int -> char option
val advance : t -> unit
val advance_by : t -> int -> unit
val take_while : t -> (char -> bool) -> string
val skip_while : t -> (char -> bool) -> unit

val take_until : t -> (char -> bool) -> string option
(** Takes characters until predicate returns true, advancing the cursor to the
    matching char. Returns None if predicate never matches.

    ## Examples

    ```ocaml match MutCursor.take_until cursor (fun c -> c = ' ') with | Some
    token -> (* got token, cursor now at space *) | None -> (* no space found *)
    ``` *)

val take_n : t -> int -> string option
(** Takes exactly n characters, advancing the cursor. Returns None if fewer than
    n remain. *)

val remaining : t -> string
