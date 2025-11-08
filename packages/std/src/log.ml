open Global
open Sync
  open Sync.Cell

type level = Trace | Debug | Info | Warn | Error

let level_to_int = function
  | Trace -> 0
  | Debug -> 1
  | Info -> 2
  | Warn -> 3
  | Error -> 4

let level_to_string = function
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

let current_level = Cell.create Info
let set_level level = current_level := level
let get_level () = !current_level
let should_log level = level_to_int level >= level_to_int !current_level
let log_file : Fs.File.t option Cell.t = Cell.create None

let set_log_file path =
  match Fs.File.open_append path with
  | Ok file -> log_file := Some file
  | Error _ -> ()

let log level msg =
  if should_log level then
    let timestamp = Datetime.to_iso8601 (Datetime.now ()) in
    let line =
      timestamp ^ " | " ^ level_to_string level ^ " | " ^ msg ^ "\n"
    in
    match !log_file with
    | Some file ->
        let _ = Fs.File.write_string file line in
        ()
    | None -> print line

let trace msg = log Trace msg
let debug msg = log Debug msg
let info msg = log Info msg
let warn msg = log Warn msg
let error msg = log Error msg
