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

module Iterator: module type of Iterator

module MutIterator: sig
  module type Intf = sig
    type state
    type item
    val next: state -> item option

    val size: state -> int

    val clone: state -> state
  end

  type ('item, 'state) iter = (module Intf with type item = 'item and type state = 'state)
  type 'item t
  val empty: unit -> 'item t

  val singleton: 'item -> 'item t

  val make: ('item, 'state) iter -> 'state -> 'item t

  val next: 'item t -> 'item option

  val size: 'item t -> int

  val clone: 'item t -> 'item t

  val collect: 'item t -> 'item list -> 'item list

  val to_list: 'item t -> 'item list

  val map: 'a t -> fn:('a -> 'b) -> 'b t

  val filter: 'a t -> fn:('a -> bool) -> 'a t

  val filter_map: 'a t -> fn:('a -> 'b option) -> 'b t

  val flat_map: 'a t -> fn:('a -> 'b t) -> 'b t

  val fold: 'a t -> init:'acc -> fn:('a -> 'acc -> 'acc) -> 'acc

  val reduce: 'a t -> fn:('a -> 'a -> 'a) -> 'a option

  val count: 'a t -> int

  val find: 'a t -> fn:('a -> bool) -> 'a option

  val any: 'a t -> fn:('a -> bool) -> bool

  val all: 'a t -> fn:('a -> bool) -> bool

  val take: 'a t -> int -> 'a t

  val drop: 'a t -> int -> 'a t

  val enumerate: 'a t -> (int * 'a) t

  val zip: 'a t -> 'b t -> ('a * 'b) t

  val chain: 'a t -> 'a t -> 'a t

  val for_each: 'a t -> fn:('a -> unit) -> unit
end

module Cursor: module type of Cursor

module MutCursor: module type of Mut_cursor
