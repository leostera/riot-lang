open Std

type t = {
  frame_rate: float;
  mutable next_frame: Time.Instant.t;
}

type tick_result =
  | Frame
  | Skip

let add_duration = fun time rate -> Time.Instant.add time (Time.Duration.from_secs_float rate)

let make = fun frame_rate ->
  (* Initialize next_frame to now, so the first tick will succeed immediately *)
  let now = Time.Instant.now () in
  { frame_rate; next_frame = now }

let from_int = fun i -> make (1.0 /. float_of_int i)

let from_float = fun f -> make (1.0 /. f)

let tick = fun ?(now = Time.Instant.now ()) t ->
  if Time.Instant.compare now t.next_frame != Order.LT then (
    (* Add frame_rate to next_frame, not to now, to maintain consistent intervals *)
    t.next_frame <- add_duration t.next_frame t.frame_rate;
    Frame
  ) else
    Skip
