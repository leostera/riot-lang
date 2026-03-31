open Kernel

type timespec = {
  secs: int;
  nanos: int;
}

type t = timespec

let epoch = {secs = 0; nanos = 0}

(* Accessors *)

let to_parts = fun t -> (t.secs, t.nanos)

let secs = fun t -> t.secs

let secs_float = fun t -> float_of_int t.secs +. (float_of_int t.nanos /. 1_000_000_000.0)

let nanos = fun t ->
    Int64.add (Int64.mul (Int64.of_int t.secs) 1_000_000_000L) (Int64.of_int t.nanos)

let from_seconds = fun f ->
    let secs = int_of_float (floor f) in
    let nanos = int_of_float ((f -. floor f) *. 1_000_000_000.0) in
    {secs; nanos}

let from_nanos = fun nanos_total ->
    let secs = Int64.to_int (Int64.div nanos_total 1_000_000_000L) in
    let nanos = Int64.to_int (Int64.rem nanos_total 1_000_000_000L) in
    {secs; nanos}

(* Creation *)

let now = fun () ->
    let time = Kernel.Time.gettimeofday () in
    let secs = int_of_float time in
    let nanos = int_of_float ((time -. float_of_int secs) *. 1_000_000_000.0) in
    {secs; nanos}

(* Duration operations *)

let duration_since = fun ~earlier later ->
    let secs_diff = later.secs - earlier.secs in
    let nanos_diff = later.nanos - earlier.nanos in
    if nanos_diff < 0 then
      Duration.make ~secs:((secs_diff - 1)) ~nanos:((nanos_diff + 1_000_000_000))
    else
      Duration.make ~secs:secs_diff ~nanos:nanos_diff

let elapsed = fun t -> duration_since ~earlier:t (now ())

(* Arithmetic operations *)

let add = fun systime duration ->
    let dur_secs = Duration.to_secs duration in
    let dur_nanos = Duration.subsec_nanos duration in
    let new_secs = systime.secs + dur_secs in
    let new_nanos = systime.nanos + dur_nanos in
    if new_nanos >= 1_000_000_000 then
      {secs = new_secs + 1; nanos = new_nanos - 1_000_000_000}
    else
      {secs = new_secs; nanos = new_nanos}

let sub = fun systime duration ->
    let dur_secs = Duration.to_secs duration in
    let dur_nanos = Duration.subsec_nanos duration in
    let new_secs = systime.secs - dur_secs in
    let new_nanos = systime.nanos - dur_nanos in
    if new_nanos < 0 then
      {secs = new_secs - 1; nanos = new_nanos + 1_000_000_000}
    else
      {secs = new_secs; nanos = new_nanos}

(* Checked operations *)

let checked_add = fun systime duration ->
    try
      let result = add systime duration in
      if result.secs >= 0 then
        Some result
      else
        None
    with
    | _ -> None

let checked_sub = fun systime duration ->
    try
      let result = sub systime duration in
      if result.secs >= 0 then
        Some result
      else
        None
    with
    | _ -> None

(* Comparison *)

let compare = fun a b ->
    let secs_cmp = compare a.secs b.secs in
    if secs_cmp = 0 then
      compare a.nanos b.nanos
    else
      secs_cmp

let equal = fun a b -> compare a b = 0

let min = fun a b ->
    if compare a b <= 0 then
      a
    else
      b

let max = fun a b ->
    if compare a b >= 0 then
      a
    else
      b

(* Unix timestamp conversion *)

let to_unix_timestamp = fun t -> t.secs

let from_unix_timestamp = fun secs -> {secs; nanos = 0}

let duration_since_epoch = fun () ->
    let t = now () in
    Duration.make ~secs:t.secs ~nanos:t.nanos
