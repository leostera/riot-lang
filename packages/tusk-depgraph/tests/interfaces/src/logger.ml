(* logger.ml - implementation *)
type level = Debug | Info | Warn | Error

let current_level = ref Info

let set_level l = current_level := l

let log level msg =
  Printf.printf "[%s] %s\n"
    (match level with Debug -> "DEBUG" | Info -> "INFO" | Warn -> "WARN" | Error -> "ERROR")
    msg