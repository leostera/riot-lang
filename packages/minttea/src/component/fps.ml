open Std

type t = { frame_rate : float; mutable next_frame : Time.Instant.t }

let add_duration time rate =
  Time.Instant.add time (Time.Duration.from_secs_float rate)

let make frame_rate =
  { frame_rate; next_frame = add_duration (Time.Instant.now ()) frame_rate }

let of_int i = make (1.0 /. float_of_int i)
let of_float f = make (1.0 /. f)

let tick ?(now = Time.Instant.now ()) t =
  if Time.Instant.compare now t.next_frame > 0 then (
    t.next_frame <- add_duration now t.frame_rate;
    `frame)
  else `skip
