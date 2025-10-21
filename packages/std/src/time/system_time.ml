type timespec = { secs : int; nanos : int }
type t = timespec

(* Creation *)
let now () =
  let time = Kernel.Time.gettimeofday () in
  let secs = int_of_float time in
  let nanos = int_of_float ((time -. float_of_int secs) *. 1_000_000_000.0) in
  { secs; nanos }

(* Duration operations *)
let duration_since ~earlier later =
  let secs_diff = later.secs - earlier.secs in
  let nanos_diff = later.nanos - earlier.nanos in
  if nanos_diff < 0 then
    Duration.make ~secs:(secs_diff - 1) ~nanos:(nanos_diff + 1_000_000_000)
  else Duration.make ~secs:secs_diff ~nanos:nanos_diff

let elapsed t = duration_since ~earlier:t (now ())

(* Arithmetic operations *)
let add systime duration =
  let dur_secs = Duration.to_secs duration in
  let dur_nanos = Duration.subsec_nanos duration in
  let new_secs = systime.secs + dur_secs in
  let new_nanos = systime.nanos + dur_nanos in
  if new_nanos >= 1_000_000_000 then
    { secs = new_secs + 1; nanos = new_nanos - 1_000_000_000 }
  else { secs = new_secs; nanos = new_nanos }

let sub systime duration =
  let dur_secs = Duration.to_secs duration in
  let dur_nanos = Duration.subsec_nanos duration in
  let new_secs = systime.secs - dur_secs in
  let new_nanos = systime.nanos - dur_nanos in
  if new_nanos < 0 then
    { secs = new_secs - 1; nanos = new_nanos + 1_000_000_000 }
  else { secs = new_secs; nanos = new_nanos }

(* Checked operations *)
let checked_add systime duration =
  try
    let result = add systime duration in
    if result.secs >= 0 then Some result else None
  with _ -> None

let checked_sub systime duration =
  try
    let result = sub systime duration in
    if result.secs >= 0 then Some result else None
  with _ -> None

(* Comparison *)
let compare a b =
  let secs_cmp = compare a.secs b.secs in
  if secs_cmp = 0 then compare a.nanos b.nanos else secs_cmp

let equal a b = compare a b = 0
let min a b = if compare a b <= 0 then a else b
let max a b = if compare a b >= 0 then a else b

(* Unix timestamp conversion *)
let to_unix_timestamp t = t.secs
let from_unix_timestamp secs = { secs; nanos = 0 }
