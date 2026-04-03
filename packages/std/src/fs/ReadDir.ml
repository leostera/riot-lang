open Global
open Common

type entry_kind =
  | Unknown
  | Regular
  | Directory
  | Symlink
  | Other

type entry = {
  path: Path.t;
  kind: entry_kind;
}

type t = {
  path: Path.t;
  handle: Kernel.Fs.ReadDir.t;
  mutable closed: bool;
}

(** Directory reading iterator *)
type state = t

type item = Path.t

let entry_kind_of_kernel = function
  | Kernel.Fs.ReadDir.Unknown -> Unknown
  | Kernel.Fs.ReadDir.Regular -> Regular
  | Kernel.Fs.ReadDir.Directory -> Directory
  | Kernel.Fs.ReadDir.Symlink -> Symlink
  | Kernel.Fs.ReadDir.Block
  | Kernel.Fs.ReadDir.Character
  | Kernel.Fs.ReadDir.Fifo
  | Kernel.Fs.ReadDir.Socket -> Other

let create = fun path ->
  let path_str = Path.to_string path in
  match Kernel.Fs.ReadDir.open_ path_str with
  | Error e -> Error e
  | Ok handle -> Ok { path; handle; closed = false }

let close = fun t ->
  if not t.closed then
    (
      t.closed <- true;
      try
        Kernel.Fs.ReadDir.close t.handle |> ignore;
        Ok ()
      with
      | e -> Error (IO.Unknown_error (Exception.to_string e))
    )
  else
    Ok ()

let rec next_entry = fun t ->
  if t.closed then
    None
  else
    try
      let entry =
        match Kernel.Fs.ReadDir.read_entry t.handle with
        | Ok e -> e
        | Error _ -> raise End_of_file
      in
      if entry.name = "." || entry.name = ".." then
        next_entry t
        (* Skip . and .. *)
      else
        match Path.of_string entry.name with
        | Ok path -> Some { path; kind = entry_kind_of_kernel entry.kind }
        | Error _ -> next_entry t
    with
    | End_of_file ->
        close t |> Result.expect ~msg:(("Could not close ReadDir.t for " ^ Path.to_string t.path));
        None

let next = fun t ->
  match next_entry t with
  | Some entry -> Some entry.path
  | None -> None

(* MutIterator.Intf implementation *)

let size = fun _t -> 0

(* Unknown size for directory iteration *)

let clone = fun t ->
  (* Can't really clone a directory handle, so we create a new one *)
  let path_str = Path.to_string t.path in
  match Kernel.Fs.ReadDir.open_ path_str with
  | Ok handle -> { path = t.path; handle; closed = false }
  | Error _ -> t

(* Fall back to the original if we can't create a new one *)
