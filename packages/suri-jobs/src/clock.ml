open Std

let now () = DateTime.to_iso8601 (DateTime.now_utc ())

let after_seconds seconds =
  DateTime.to_iso8601
    (DateTime.from_system_time
      (Time.SystemTime.add (Time.SystemTime.now ()) (Time.Duration.from_secs seconds)))

let before_seconds seconds =
  DateTime.to_iso8601
    (DateTime.from_system_time
      (Time.SystemTime.sub (Time.SystemTime.now ()) (Time.Duration.from_secs seconds)))

let lte left right =
  match String.compare left right with
  | Order.LT
  | Order.EQ -> true
  | Order.GT -> false
