(** # Iter - Iteration and Cursor Utilities

    This module provides iteration and cursor abstractions for traversing
    sequences and strings.

    ## Iterators

    Iterators provide lazy, composable sequence processing:

    - {!Iterator} - Immutable iterators (functional, backtrackable)
    - {!MutIterator} - Mutable iterators (efficient, single-pass)

    ## Cursors

    Cursors provide string traversal for parsing:

    - {!Cursor} - Immutable cursors (backtrackable parsing)
    - {!MutCursor} - Mutable cursors (efficient single-pass parsing)

    ## Quick Start

    Immutable parsing with {!Cursor}:

    ```ocaml open Std

    let parse_header line = let cursor = Iter.Cursor.create line in match
    Iter.Cursor.find_char cursor ':' with | None -> Error "Invalid header" |
    Some offset -> let key = Iter.Cursor.slice cursor 0 offset |> Option.unwrap
    in let cursor = Iter.Cursor.advance_by cursor (offset + 1) |> Option.unwrap
    in let (value, _) = Iter.Cursor.take_while cursor (fun c -> c <> '\r') in Ok
    (String.trim key, String.trim value) ```

    Efficient parsing with {!MutCursor}:

    ```ocaml let parse_request_line line = let cursor = Iter.MutCursor.create
    line in let method_ = Iter.MutCursor.take_while cursor (fun c -> c <> ' ')
    in Iter.MutCursor.advance cursor; let path = Iter.MutCursor.take_while
    cursor (fun c -> c <> ' ') in Iter.MutCursor.advance cursor; let version =
    Iter.MutCursor.remaining cursor in (method_, path, version) ```

    ## When to Use What

    | Use Case | Recommendation | |----------|----------------| | Backtracking
    parsers | {!Cursor} | | Single-pass parsers | {!MutCursor} | | Functional
    style | {!Cursor}, {!Iterator} | | Performance-critical | {!MutCursor},
    {!MutIterator} | | Lazy sequences | {!Iterator}, {!MutIterator} | | String
    parsing | {!Cursor}, {!MutCursor} | *)

module Iterator : module type of Iterator
(** Immutable iterator protocol for lazy sequences *)

module MutIterator : module type of MutIterator
(** Mutable iterator protocol for efficient iteration *)

module Cursor : module type of Cursor
(** Immutable string cursor for backtrackable parsing *)

module MutCursor : module type of Mut_cursor
(** Mutable string cursor for efficient single-pass parsing *)
