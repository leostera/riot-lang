open Prelude

let ( let* ) = Result.and_then

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
  name: string;
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
  | File error -> String.concat
    ""
    [ "file error while reading directory entry: "; File.error_to_string error ]

let next_name = fun dir ->
  if dir.index >= Array.length dir.names then
    None
  else
    let name = Array.get dir.names dir.index in
    dir.index <- dir.index + 1;
    Some name

let open_dir = fun path ->
  let* names =
    Result.map_error (fun error -> File error) (File.read_dir_names path)
  in
  Result.Ok { root = path; names; index = 0; closed = false }

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
        let path = Path.join dir.root (Path.of_string name) in
        let* metadata =
          Result.map_error (fun error -> File error) (File.symlink_metadata path)
        in
        Result.Ok (Some { name; kind = File.Metadata.file_type metadata })

let close = fun dir ->
  dir.closed <- true;
  Result.Ok ()
