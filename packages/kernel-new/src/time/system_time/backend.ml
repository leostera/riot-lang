open Prelude

let ( let* ) = Result.and_then

type error =
  | Invalid_nanoseconds of { nanos: int }
  | System of System_error.t

type t = {
  secs: int;
  nanos: int;
}

let epoch = { secs = 0; nanos = 0 }

module FFI = struct
  external now: unit -> ((int * int), int) Result.t = "kernel_new_time_system_time_now"
end

let error_to_string = function
  | Invalid_nanoseconds { nanos } -> String.concat
    ""
    [ "invalid nanoseconds component: "; Int.to_string nanos ]
  | System error -> System_error.to_string error

let validate_parts = fun ~secs:_ ~nanos ->
  if nanos < 0 || nanos >= 1_000_000_000 then
    Result.Error (Invalid_nanoseconds { nanos })
  else
    Result.Ok ()

let of_parts = fun ~secs ~nanos ->
  let* () = validate_parts ~secs ~nanos in
  Result.Ok { secs; nanos }

let of_parts_unchecked = fun ~secs ~nanos -> { secs; nanos }

let to_parts = fun value -> (value.secs, value.nanos)

let secs = fun value -> value.secs

let subsec_nanos = fun value -> value.nanos

let now = fun () ->
  let* (secs, nanos) =
    Result.map_error (fun code -> System (System_error.of_code code)) (FFI.now ())
  in
  of_parts ~secs ~nanos

let compare = fun left right ->
  let secs_order = Int.compare left.secs right.secs in
  if secs_order = 0 then
    Int.compare left.nanos right.nanos
  else
    secs_order

let equal = fun left right -> compare left right = 0
