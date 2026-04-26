open Prelude

let ( let* ) value fn = Result.and_then value ~fn

type kind = File.kind =
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
  kind: kind;
}

type t = {
  root: Path.t;
  names: string array;
  mutable index: int;
  mutable closed: bool;
}

type error =
  | Closed
  | File of File.error

let error_to_string = fun value ->
  match value with
  | Closed -> "directory iterator is closed"
  | File error ->
      String.concat "" [ "file error while reading directory entry: "; File.error_to_string error ]

let next_name = fun dir ->
  if dir.index >= Array.length dir.names then
    None
  else
    let name = Array.get_unchecked dir.names ~at:dir.index in
    dir.index <- dir.index + 1;
  Some name

let open_dir = fun path ->
  let* names =
    File.read_dir_names path
    |> Result.map_err ~fn:(fun error -> File error)
  in
  Result.Ok {
    root = path;
    names;
    index = 0;
    closed = false;
  }

let read_name = fun dir ->
  if dir.closed then
    Result.Error Closed
  else
    Result.Ok (next_name dir)

let read_entry = fun dir ->
  if dir.closed then
    Result.Error Closed
  else
    match next_name dir with
    | None -> Result.Ok None
    | Some name ->
        let path = Path.from_string name in
        let* metadata =
          File.symlink_metadata Path.(dir.root / path)
          |> Result.map_err ~fn:(fun error -> File error)
        in
        Result.Ok (Some { path; kind = File.Metadata.file_type metadata })

let close = fun dir ->
  dir.closed <- true;
  Result.Ok ()
