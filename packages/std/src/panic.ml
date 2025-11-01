(** Panic with message - backtrace will be captured by process exception handler *)
let panic msg =
  let exception Panic of string in
  raise (Panic msg)

