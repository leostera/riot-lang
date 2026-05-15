open Global
open Common

type entry_kind = Kernel.Fs.ReadDir.kind =
  | RegularFile
  | Directory
  | SymbolicLink
  | CharacterDevice
  | BlockDevice
  | NamedPipe
  | Socket
  | Unknown

type entry = {
  path: Path.t;
  kind: entry_kind;
}

type t = {
  path: Path.t;
  path_string: string;
  handle: Kernel.Fs.ReadDir.t;
  mutable closed: bool;
}

type state = t

type item = entry

let open_dir = fun path ->
  let path_string = Path.to_string path in
  match Kernel.Fs.ReadDir.open_dir path_string with
  | Ok handle ->
      Ok {
        path;
        path_string;
        handle;
        closed = false;
      }
  | Error error -> Error (from_read_dir_error error)

let close = fun dir ->
  if dir.closed then
    Ok ()
  else (
    dir.closed <- true;
    match Kernel.Fs.ReadDir.close dir.handle with
    | Ok () -> Ok ()
    | Error error -> Error (from_read_dir_error error)
  )

let next_result = fun dir ->
  if dir.closed then
    Error IO.Closed
  else
    match Kernel.Fs.ReadDir.read_entry dir.handle with
    | Ok None ->
        close dir
        |> Result.map ~fn:(fun () -> None)
    | Ok (Some entry) ->
        Ok (Some { path = Path.from_string_unchecked entry.path; kind = entry.kind })
    | Error error ->
        let read_error = from_read_dir_error error in
        let _ = close dir in
        Error read_error

let next = fun dir ->
  match next_result dir with
  | Ok entry -> entry
  | Error _ -> None

let size = fun _ -> 0

let clone = fun dir ->
  match open_dir dir.path with
  | Ok clone -> clone
  | Error _ -> dir
