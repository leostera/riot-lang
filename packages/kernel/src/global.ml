(** Common types re-exported from Stdlib for use in nostdlib packages *)
(* Include basic primitives *)

include Global0

open IO

(** Async-safe write to stdout using Fs.File.write which handles Would_block *)
let write_stdout = fun str ->
    let bytes = Bytes.unsafe_of_string str in
    let len = String.length str in
    let rec write_all offset remaining =
      if remaining = 0 then
        ()
      else
        match Fs.File.write IO.stdout ~pos:offset ~len:remaining bytes with
        | Ok n -> write_all (offset + n) (remaining - n)
        | Error _ -> ()
    in
    write_all 0 len

(** Async-safe write to stderr using Fs.File.write which handles Would_block *)
let write_stderr = fun str ->
    let bytes = Bytes.unsafe_of_string str in
    let len = String.length str in
    let rec write_all offset remaining =
      if remaining = 0 then
        ()
      else
        match Fs.File.write IO.stderr ~pos:offset ~len:remaining bytes with
        | Ok n -> write_all (offset + n) (remaining - n)
        | Error _ -> ()
    in
    write_all 0 len

(** Async-safe print to stdout - never raises Sys_blocked_io *)
let print = write_stdout

(** Async-safe print to stdout with newline - never raises Sys_blocked_io *)
let println = fun str -> write_stdout (str ^ "\n")

(** Async-safe print to stderr - never raises Sys_blocked_io *)
let eprint = write_stderr

(** Async-safe print to stderr with newline - never raises Sys_blocked_io *)
let eprintln = fun str -> write_stderr (str ^ "\n")
