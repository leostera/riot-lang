open Global

type reader

type error =
  | Invalid_header of string
  | Entry_in_progress
  | Invalid_state of string
  | Unexpected_eof
  | Out_of_memory
  | Unknown_error of string

type entry_kind =
  | File
  | Directory
  | Symlink
  | Hardlink
  | Other of string

type header = {
  path: string;
  kind: entry_kind;
  size: int64;
  mode: int option;
  link_target: string option;
}

type next =
  | Need_input
  | Entry of header
  | End

type read_result =
  | Need_input
  | Chunk of int
  | End_of_entry

type skip_result =
  | Need_input
  | Skipped

val create_reader: unit -> (reader, error) result

val feed_reader: reader -> src:bytes -> src_pos:int -> src_len:int -> (int, error) result

val next_entry: reader -> (next, error) result

val read_entry_data: reader -> dst:bytes -> dst_pos:int -> dst_len:int -> (read_result, error) result

val skip_entry: reader -> (skip_result, error) result

val close_reader: reader -> unit
