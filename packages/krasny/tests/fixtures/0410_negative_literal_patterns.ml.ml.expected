let classify_status code =
  match code with
  | -2 -> `Retry
  | -1 -> `Read
  | 0 -> `Done
  | _ -> `Unknown
