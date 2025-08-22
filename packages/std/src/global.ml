(** Panic exception and function *)

let panic msg =
  let exception Panic of string in
  raise (Panic msg)

(** Get the number of available CPU cores for parallelism *)
let available_parallelism () =
  match Sys.os_type with
  | "Unix" -> (
      try
        let ic =
          Unix.open_process_in
            "nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4"
        in
        let cores = input_line ic |> int_of_string in
        ignore (Unix.close_process_in ic);
        cores
      with _ -> 4)
  | _ -> 4

let cpu_count = available_parallelism
let os_type () = Sys.os_type
let time () = Unix.time ()
let gettimeofday () = Unix.gettimeofday ()
let time_ms () = int_of_float (Unix.gettimeofday () *. 1000.)

(** Date and time utilities *)
module Datetime = struct
  let now () = Unix.gettimeofday ()
  let localtime timestamp = Unix.localtime timestamp
  let gmtime timestamp = Unix.gmtime timestamp
end

(** Process status types *)
module Process = struct
  type status = Exited of int | Signaled of int | Stopped of int

  let of_unix_status = function
    | Unix.WEXITED code -> Exited code
    | Unix.WSIGNALED signal -> Signaled signal
    | Unix.WSTOPPED signal -> Stopped signal
end

(** File types *)
module File = struct
  type kind = Regular | Directory | Character | Block | Link | Fifo | Socket

  let kind_of_unix = function
    | Unix.S_REG -> Regular
    | Unix.S_DIR -> Directory
    | Unix.S_CHR -> Character
    | Unix.S_BLK -> Block
    | Unix.S_LNK -> Link
    | Unix.S_FIFO -> Fifo
    | Unix.S_SOCK -> Socket
end
