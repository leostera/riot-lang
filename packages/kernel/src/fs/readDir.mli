open Global0
open Async

type entry_kind =
  | Unknown
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket
type entry = {
  name: string;
  kind: entry_kind;
}
type t
val open_: string -> (t, IO.error) result

val read_entry: t -> (entry, IO.error) result

val read: t -> (string, IO.error) result

val close: t -> (unit, IO.error) result
