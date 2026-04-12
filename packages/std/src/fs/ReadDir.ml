open Global
open Common

type entry_kind =
  | Unknown
  | Regular
  | Directory
  | Symlink
  | Other

type raw_entry = {
  name: string;
  kind: entry_kind;
}

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

type item = Path.t

let entry_kind_of_kernel = function
  | Kernel.Fs.ReadDir.RegularFile -> Regular
  | Kernel.Fs.ReadDir.Directory -> Directory
  | Kernel.Fs.ReadDir.SymbolicLink -> Symlink
  | Kernel.Fs.ReadDir.CharacterDevice
  | Kernel.Fs.ReadDir.BlockDevice
  | Kernel.Fs.ReadDir.NamedPipe
  | Kernel.Fs.ReadDir.Socket -> Other
  | Kernel.Fs.ReadDir.Unknown -> Unknown

let create = fun path ->
  let path_string = Path.to_string path in
  match Kernel.Fs.ReadDir.open_dir path_string with
  | Ok handle -> Ok { path; path_string; handle; closed = false }
  | Error error -> Error (of_read_dir_error error)

let create_string = fun path_string ->
  match Path.from_string path_string with
  | Ok path -> create path
  | Error (Path.InvalidUtf8 { path }) -> Error (IO.Unknown_error ("invalid UTF-8 path: " ^ path))
  | Error (Path.SystemInvalidUtf8 { syscall; path }) -> Error (IO.Unknown_error ("invalid UTF-8 path from "
  ^ syscall
  ^ ": "
  ^ path))
  | Error (Path.SystemError message) -> Error (IO.Unknown_error message)

let close = fun dir ->
  if dir.closed then
    Ok ()
  else (
    dir.closed <- true;
    match Kernel.Fs.ReadDir.close dir.handle with
    | Ok () -> Ok ()
    | Error error -> Error (of_read_dir_error error)
  )

let next_raw_entry = fun dir ->
  if dir.closed then
    None
  else
    match Kernel.Fs.ReadDir.read_entry dir.handle with
    | Ok None ->
        let _ = close dir in
        None
    | Ok (Some entry) ->
        Some { name = entry.name; kind = entry_kind_of_kernel entry.kind }
    | Error _ ->
        let _ = close dir in
        None

let rec next_entry = fun dir ->
  match next_raw_entry dir with
  | Some entry -> (
      match Path.from_string entry.name with
      | Ok path -> Some { path; kind = entry.kind }
      | Error _ -> next_entry dir
    )
  | None -> None

let next = fun dir ->
  match next_entry dir with
  | Some entry -> Some entry.path
  | None -> None

let size = fun _ -> 0

let clone = fun dir ->
  match create_string dir.path_string with
  | Ok clone -> clone
  | Error _ -> dir
