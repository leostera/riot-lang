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

external open_handle: string -> t = "kernel_fs_read_dir_open"

external read_entry_raw: t -> entry = "kernel_fs_read_dir_read_entry"

external close_handle: t -> unit = "kernel_fs_read_dir_close"

let open_ = fun path ->
  try Ok (open_handle path) with
  | Unix.Unix_error (e, _, _) -> Error (IO.error_of_unix e)

let read_entry = fun handle ->
  try Ok (read_entry_raw handle) with
  | End_of_file -> Error IO.End_of_file
  | Unix.Unix_error (e, _, _) -> Error (IO.error_of_unix e)

let read = fun handle ->
  match read_entry handle with
  | Ok entry -> Ok entry.name
  | Error err -> Error err

let close = fun handle ->
  try
    close_handle handle;
    Ok ()
  with
  | Unix.Unix_error (e, _, _) -> Error (IO.error_of_unix e)
