open Prelude

type error = Error.t

type t = {
  secs: int;
  nanos: int;
}

let epoch = { secs = 0; nanos = 0 }

module FFI = struct
  external now:
    unit -> ((int * int), int) Result.t
    = "kernel_new_time_system_time_now"
end

let validate_parts = fun ~secs:_ ~nanos ->
  if nanos < 0 || nanos >= 1_000_000_000 then
    Error.panic "system time nanoseconds must be between 0 and 999999999"

let of_parts = fun ~secs ~nanos ->
  validate_parts ~secs ~nanos;
  { secs; nanos }

let to_parts = fun value -> (value.secs, value.nanos)

let secs = fun value -> value.secs

let subsec_nanos = fun value -> value.nanos

let now = fun () ->
  Result.map_error
    Error.of_code
    (Result.map (fun (secs, nanos) -> of_parts ~secs ~nanos) (FFI.now ()))

let compare = fun left right ->
  let secs_order = Int.compare left.secs right.secs in
  if secs_order = 0 then
    Int.compare left.nanos right.nanos
  else
    secs_order

let equal = fun left right -> compare left right = 0
