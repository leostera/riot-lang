open Global

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

let current_level = ref Info
let set_level level = current_level := level
let get_level () = !current_level
let should_log level = level_to_int level >= level_to_int !current_level

let log level fmt =
  if should_log level then
    Printf.ksprintf
      (fun msg ->
        let timestamp = Datetime.to_iso8601 (Datetime.now ()) in
        Printf.printf "%s | %s | %s\n%!" timestamp (level_to_string level) msg)
      fmt
  else Printf.ifprintf () fmt

let trace fmt = log Trace fmt
let debug fmt = log Debug fmt
let info fmt = log Info fmt
let warn fmt = log Warn fmt
let error fmt = log Error fmt
