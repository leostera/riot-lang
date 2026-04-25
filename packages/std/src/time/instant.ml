open Kernel

let panic = Kernel.SystemError.panic

type timespec = { secs: int; nanos: int }

type t = timespec

(* Creation *)
let now = fun () ->
  match Kernel.Time.Monotonic.now () with
  | Ok time ->
      let secs, nanos = Kernel.Time.Monotonic.to_parts time in { secs; nanos }
  | Error err -> Kernel.SystemError.panic (Kernel.Time.Monotonic.error_to_string err)

(* Duration operations *)
let duration_since = fun ~earlier later ->
  let secs_diff = later.secs - earlier.secs in
  let nanos_diff = later.nanos - earlier.nanos in
  if secs_diff < 0 || (secs_diff = 0 && nanos_diff < 0) then
    panic "Instant.duration_since called with earlier > later"
  else
    if nanos_diff < 0 then
      Duration.make ~secs:(secs_diff - 1) ~nanos:(nanos_diff + 1_000_000_000)
    else Duration.make ~secs:secs_diff ~nanos:nanos_diff

let saturating_duration_since = fun ~earlier later ->
  let secs_diff = later.secs - earlier.secs in
  let nanos_diff = later.nanos - earlier.nanos in
  if secs_diff < 0 || (secs_diff = 0 && nanos_diff < 0) then
    Duration.zero
  else
    if nanos_diff < 0 then
      Duration.make ~secs:(secs_diff - 1) ~nanos:(nanos_diff + 1_000_000_000)
    else Duration.make ~secs:secs_diff ~nanos:nanos_diff

let elapsed = fun t -> duration_since ~earlier:t (now ())

(* Arithmetic operations *)
let add = fun instant duration ->
  let dur_secs = Duration.to_secs duration in
  let dur_nanos = Duration.subsec_nanos duration in
  let new_secs = instant.secs + dur_secs in
  let new_nanos = instant.nanos + dur_nanos in
  if new_nanos >= 1_000_000_000 then
    { secs = new_secs + 1; nanos = new_nanos - 1_000_000_000 }
  else { secs = new_secs; nanos = new_nanos }

let sub = fun instant duration ->
  let dur_secs = Duration.to_secs duration in
  let dur_nanos = Duration.subsec_nanos duration in
  let new_secs = instant.secs - dur_secs in
  let new_nanos = instant.nanos - dur_nanos in
  if new_nanos < 0 then
    { secs = new_secs - 1; nanos = new_nanos + 1_000_000_000 }
  else { secs = new_secs; nanos = new_nanos }

(* Checked operations *)
let checked_add = fun instant duration ->
  try
    let result = add instant duration in
    if result.secs >= 0 then
      Some result
    else None
  with
  | _ -> None

let checked_sub = fun instant duration ->
  try
    let result = sub instant duration in
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
