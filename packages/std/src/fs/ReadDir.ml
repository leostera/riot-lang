open Common

type t = { path : Path.t; handle : Unix.dir_handle; mutable closed : bool }
(** Directory reading iterator *)

type state = t
type item = Path.t

let create path =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.opendir path_str with
  | Error e -> Error (SystemError (kernel_error_to_string e))
  | Ok handle -> Ok { path; handle; closed = false }

let close t =
  if not t.closed then (
    t.closed <- true;
    try
      Kernel.Fs.File.closedir t.handle |> ignore;
      Ok ()
    with e -> Error (SystemError (Printexc.to_string e)))
  else Ok ()

let rec next t =
  if t.closed then None
  else
    try
      let entry =
        match Kernel.Fs.File.readdir_handle t.handle with
        | Ok e -> e
        | Error _ -> raise End_of_file
      in
      if entry = "." || entry = ".." then next t (* Skip . and .. *)
      else
        match Path.of_string entry with
        | Ok p -> Some p
        | Error _ -> next t (* Skip invalid paths *)
    with End_of_file ->
      close t
      |> Result.expect
           ~msg:
             (Format.sprintf "Could not close ReadDir.t for %S"
                (Path.to_string t.path));
      None

(* MutIterator.Intf implementation *)
let size _t = 0 (* Unknown size for directory iteration *)

let clone t =
  (* Can't really clone a directory handle, so we create a new one *)
  let path_str = Path.to_string t.path in
  match Kernel.Fs.File.opendir path_str with
  | Ok handle -> { path = t.path; handle; closed = false }
  | Error _ -> t (* Fall back to the original if we can't create a new one *)
