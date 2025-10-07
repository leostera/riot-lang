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
let log_file : Fs.File.t option ref = ref None

let set_log_file path =
  match Fs.File.open_append path with
  | Ok file -> log_file := Some file
  | Error _ -> ()

let log level fmt =
  if should_log level then
    Printf.ksprintf
      (fun msg ->
        let timestamp = Datetime.to_iso8601 (Datetime.now ()) in
        let line =
          format "%s | %s | %s\n" timestamp (level_to_string level) msg
        in
        match !log_file with
        | Some file ->
            let _ = Fs.File.write_string file line in
            ()
        | None -> Printf.printf "%s%!" line)
      fmt
  else Printf.ifprintf () fmt

let trace fmt = log Trace fmt
let debug fmt = log Debug fmt
let info fmt = log Info fmt
let warn fmt = log Warn fmt
let error fmt = log Error fmt
