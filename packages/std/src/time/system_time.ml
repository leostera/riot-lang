open Kernel

type timespec = { secs: int; nanos: int }

type t = timespec

let epoch = { secs = 0; nanos = 0 }

(* Accessors *)
let to_parts = fun t -> (t.secs, t.nanos)

let secs = fun t -> t.secs

let secs_float = fun t -> Float.from_int t.secs +. (Float.from_int t.nanos /. 1_000_000_000.0)

let nanos = fun t -> Int64.add (Int64.mul (Int64.from_int t.secs) 1_000_000_000L) (Int64.from_int t.nanos)

let from_seconds = fun f ->
  let whole = Float.floor f in
  let secs = Int.from_float whole in
  let nanos = Int.from_float ((f -. whole) *. 1_000_000_000.0) in { secs; nanos }

let from_nanos = fun nanos_total ->
  let secs = Int64.to_int (Int64.div nanos_total 1_000_000_000L) in
  let nanos = Int64.to_int (Int64.rem nanos_total 1_000_000_000L) in { secs; nanos }

(* Creation *)
let now = fun () ->
  match Kernel.Time.SystemTime.now () with
  | Ok time ->
      let secs, nanos = Kernel.Time.SystemTime.to_parts time in { secs; nanos }
  | Error err -> Kernel.SystemError.panic (Kernel.Time.SystemTime.error_to_string err)

(* Duration operations *)
let duration_since = fun ~earlier later ->
  let secs_diff = later.secs - earlier.secs in
  let nanos_diff = later.nanos - earlier.nanos in
  if nanos_diff < 0 then
    Duration.make ~secs:(secs_diff - 1) ~nanos:(nanos_diff + 1_000_000_000)
  else Duration.make ~secs:secs_diff ~nanos:nanos_diff

let elapsed = fun t -> duration_since ~earlier:t (now ())

(* Arithmetic operations *)
let add = fun systime duration ->
  let dur_secs = Duration.to_secs duration in
  let dur_nanos = Duration.subsec_nanos duration in
  let new_secs = systime.secs + dur_secs in
  let new_nanos = systime.nanos + dur_nanos in
  if new_nanos >= 1_000_000_000 then
    { secs = new_secs + 1; nanos = new_nanos - 1_000_000_000 }
  else { secs = new_secs; nanos = new_nanos }

let sub = fun systime duration ->
  let dur_secs = Duration.to_secs duration in
  let dur_nanos = Duration.subsec_nanos duration in
  let new_secs = systime.secs - dur_secs in
  let new_nanos = systime.nanos - dur_nanos in
  if new_nanos < 0 then
    { secs = new_secs - 1; nanos = new_nanos + 1_000_000_000 }
  else { secs = new_secs; nanos = new_nanos }

(* Checked operations *)
let checked_add = fun systime duration ->
  try
    let result = add systime duration in
    if result.secs >= 0 then
      Some result
    else None
  with
  | _ -> None

let checked_sub = fun systime duration ->
  try
    let result = sub systime duration in
    if result.secs >= 0 then
      Some result
    else None
  with
  | _ -> None

(* Comparison *)
let compare = fun a b ->
  let secs_cmp = Int.compare a.secs b.secs in
  match secs_cmp with
  | Order.EQ -> Int.compare a.nanos b.nanos
  | Order.LT | Order.GT -> secs_cmp

let equal = fun a b ->
  match compare a b with
  | Order.EQ -> true
  | Order.LT | Order.GT -> false

let min = fun a b ->
  match compare a b with
  | Order.LT | Order.EQ -> a
  | Order.GT -> b

let max = fun a b ->
  match compare a b with
  | Order.LT -> b
  | Order.EQ | Order.GT -> a

(* Unix timestamp conversion *)
let to_unix_timestamp = fun t -> t.secs

let from_unix_timestamp = fun secs -> { secs; nanos = 0 }

let duration_since_epoch = fun () ->
  let t = now () in Duration.make ~secs:t.secs ~nanos:t.nanos
