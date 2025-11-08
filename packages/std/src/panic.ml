(** Panic with message - backtrace will be captured by process exception handler *)

open Global

let panic msg =
  let exception Panic of string in
  raise (Panic msg)

