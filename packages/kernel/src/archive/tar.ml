open Global0
open IO

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

type raw_header = string * int * string option * int64 * int option * string option

external _create_reader: unit -> reader = "kernel_tar_create_reader"

external _feed_reader_raw: reader -> bytes -> int -> int -> int = "kernel_tar_feed_reader"

external _next_entry_raw:
  reader ->
  int * int * raw_header option = "kernel_tar_next_entry"

external _read_entry_data_raw:
  reader ->
  bytes ->
  int ->
  int ->
  int * int * int = "kernel_tar_read_entry_data"

external _skip_entry_raw: reader -> int * int = "kernel_tar_skip_entry"

external _close_reader: reader -> unit = "kernel_tar_close_reader"

let error_of_code = function
  | 0 -> None
  | 1 -> Some (Invalid_header "invalid tar header")
  | 2 -> Some Entry_in_progress
  | 3 -> Some (Invalid_state "invalid tar reader state")
  | 4 -> Some Unexpected_eof
  | 5 -> Some Out_of_memory
  | code -> Some (Unknown_error ("unknown tar error code " ^ Int.to_string code))

let check_slice = fun label buffer ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buffer then
    Stdlib.invalid_arg (label ^ ": invalid slice")

let entry_kind_of_raw = fun kind other ->
  match kind with
  | 0 -> File
  | 1 -> Directory
  | 2 -> Symlink
  | 3 -> Hardlink
  | 4 -> Other (Option.unwrap_or other ~default:"")
  | code -> panic ("invalid tar entry kind code " ^ Int.to_string code)

let header_of_raw = fun (path, kind, other, size, mode, link_target) ->
  { path; kind = entry_kind_of_raw kind other; size; mode; link_target }

let create_reader = fun () ->
  try Ok (_create_reader ()) with
  | Failure msg -> Error (Unknown_error msg)

let feed_reader = fun reader ~src ~src_pos ~src_len ->
  check_slice "Kernel.Archive.Tar.feed_reader" src ~pos:src_pos ~len:src_len;
  try Ok (_feed_reader_raw reader src src_pos src_len) with
  | Failure msg -> Error (Unknown_error msg)

let next_entry : reader -> (next, error) result = fun reader ->
  let (error_code, status_code, raw_header) = _next_entry_raw reader in
  match error_of_code error_code with
  | Some err -> Error err
  | None -> (
      match (status_code, raw_header) with
      | 0, _ -> Ok Need_input
      | 1, Some header -> Ok (Entry (header_of_raw header))
      | 2, _ -> Ok End
      | _, _ -> Error (Unknown_error "invalid tar next_entry response")
    )

let read_entry_data : reader -> dst:bytes -> dst_pos:int -> dst_len:int -> (read_result, error) result =
    fun reader ~dst ~dst_pos ~dst_len ->
  check_slice "Kernel.Archive.Tar.read_entry_data" dst ~pos:dst_pos ~len:dst_len;
  let (error_code, status_code, produced) = _read_entry_data_raw reader dst dst_pos dst_len in
  match error_of_code error_code with
  | Some err -> Error err
  | None -> (
      match status_code with
      | 0 -> Ok Need_input
      | 1 -> Ok (Chunk produced)
      | 2 -> Ok End_of_entry
      | _ -> Error (Unknown_error "invalid tar read_entry_data response")
    )

let skip_entry : reader -> (skip_result, error) result = fun reader ->
  let (error_code, status_code) = _skip_entry_raw reader in
  match error_of_code error_code with
  | Some err -> Error err
  | None -> (
      match status_code with
      | 0 -> Ok Need_input
      | 1 -> Ok Skipped
      | _ -> Error (Unknown_error "invalid tar skip_entry response")
    )

let close_reader = fun reader -> _close_reader reader
