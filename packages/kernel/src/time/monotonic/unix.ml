open Prelude

let ( let* ) value fn = Result.and_then value ~fn

type error =
  | InvalidNanoseconds of { nanos: int }
  | System of System_error.t

type t = {
  secs: int;
  nanos: int;
}

module FFI = struct
  external now: unit -> ((int * int), int) Result.t = "kernel_new_time_monotonic_now"
end

let error_to_string error =
  match error with
  | InvalidNanoseconds { nanos } -> String.concat
    ""
    [ "invalid nanoseconds component: "; Int.to_string nanos ]
  | System system_error -> System_error.to_string system_error

let validate_parts = fun ~secs:_ ~nanos ->
  Result.map_err (Common.validate_nanos nanos) ~fn:(fun () -> InvalidNanoseconds { nanos })

let from_parts = fun ~secs ~nanos ->
  let* () = validate_parts ~secs ~nanos in
  Result.Ok { secs; nanos }

let to_parts = fun value -> (value.secs, value.nanos)

let secs = fun value -> value.secs

let subsec_nanos = fun value -> value.nanos

let now = fun () ->
  let* (secs, nanos) =
    Result.map_err (FFI.now ()) ~fn:(fun code -> System (System_error.from_code code))
  in
  from_parts ~secs ~nanos

let compare = fun left right ->
  Common.compare_parts
    ~left_secs:left.secs
    ~left_nanos:left.nanos
    ~right_secs:right.secs
    ~right_nanos:right.nanos

let equal = fun left right -> compare left right = 0

let diff_ns = fun left right ->
  Common.diff_ns
    ~left_secs:left.secs
    ~left_nanos:left.nanos
    ~right_secs:right.secs
    ~right_nanos:right.nanos
