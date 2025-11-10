open Global0
  open IO
  open Collections

let syscall = Async.syscall

type t = int * Fd.t  (* context_ptr, read_fd *)
type watch_id = int

type event = {
  path : string;
  flags : int32;
}

type event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

(* Platform-specific FFI - macOS FSEvents *)
external fsevents_create : unit -> int * Fd.t = "kernel_fsevents_create"
external fsevents_watch : int -> string -> float -> unit = "kernel_fsevents_watch"
external fsevents_stop : int -> unit = "kernel_fsevents_stop"

(* FSEvents flag constants *)
let flag_created = 0x00000100l
let flag_removed = 0x00000200l
let flag_modified = 0x00001000l
let flag_renamed = 0x00000800l
let flag_metadata = 0x00004000l

let decode_event_kind flags =
  let has_flag flag = Int32.logand flags flag != Int32.zero in
  if has_flag flag_created then Created
  else if has_flag flag_removed then Deleted
  else if has_flag flag_modified then Modified
  else if has_flag flag_renamed then Renamed
  else Metadata

let create () =
  syscall @@ fun () ->
  let (ctx_ptr, read_fd) = fsevents_create () in
  Ok (ctx_ptr, read_fd)

let watch t ~path ~latency =
  syscall @@ fun () ->
  let (ctx_ptr, _read_fd) = t in
  fsevents_watch ctx_ptr path latency;
  Ok 0  (* FSEvents doesn't return per-path IDs *)

let unwatch _t _watch_id =
  (* FSEvents watches are stopped when context is destroyed *)
  Ok ()

let read_events t =
  syscall @@ fun () ->
  let (_ctx_ptr, read_fd) = t in
  
  (* Read events from pipe - format: path_len (4 bytes) | flags (4 bytes) | path *)
  let rec read_all acc =
    (* Try to read path_len (4 bytes) *)
    let len_buf = Bytes.create 4 in
    match File.read read_fd len_buf ~len:4 with
    | Error End_of_file -> Ok (List.rev acc)
    | Ok 0 -> Ok (List.rev acc)  (* No data available *)
    | Error e -> Error e
    | Ok n when n < 4 -> Ok (List.rev acc)  (* Incomplete read, return what we have *)
    | Ok _ ->
        (* Read flags (4 bytes) *)
        let flags_buf = Bytes.create 4 in
        (match File.read read_fd flags_buf ~len:4 with
        | Error _ -> Ok (List.rev acc)
        | Ok n when n < 4 -> Ok (List.rev acc)
        | Ok _ ->
            let path_len = Int32.to_int (Bytes.get_int32_ne len_buf 0) in
            let flags = Bytes.get_int32_ne flags_buf 0 in
            
            (* Read path *)
            let path_buf = Bytes.create path_len in
            (match File.read read_fd path_buf ~len:path_len with
            | Error _ -> Ok (List.rev acc)
            | Ok n when n < path_len -> Ok (List.rev acc)
            | Ok _ ->
                let path = Bytes.to_string path_buf in
                read_all ({ path; flags } :: acc)))
  in
  read_all []

let stop t =
  syscall @@ fun () ->
  let (ctx_ptr, read_fd) = t in
  fsevents_stop ctx_ptr;
  File.close_fd read_fd;
  Ok ()
