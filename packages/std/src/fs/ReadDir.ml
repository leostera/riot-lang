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

let next = fun dir ->
  if dir.closed then
    None
  else
    match Kernel.Fs.ReadDir.read_entry dir.handle with
    | Ok None ->
        let _ = close dir in
        None
    | Ok (Some entry) -> Some { path = Path.from_string_unchecked entry.path; kind = entry.kind }
    | Error _ ->
        let _ = close dir in
        None

let size = fun _ -> 0

let clone = fun dir ->
  match open_dir dir.path with
  | Ok clone -> clone
  | Error _ -> dir
