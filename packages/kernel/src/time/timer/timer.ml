open Prelude

type error =
  | InvalidTimeoutNs of { timeout_ns: int64 }

type t = {
  id: int;
  timeout_ns: int64;
  timeout_secs: int;
  timeout_nanos: int;
  repeat: bool;
}

type next_id = {
  mutable value: int;
}

let next_id = { value = 0 }

let error_to_string error =
  match error with
  | InvalidTimeoutNs { timeout_ns=_ } -> "invalid timer timeout"

let fresh_id = fun () ->
  next_id.value <- next_id.value + 1;
  next_id.value

let make = fun ~repeat timeout_ns ->
  if timeout_ns <= 0L then
    Result.Error (InvalidTimeoutNs { timeout_ns })
  else
    let (timeout_secs, timeout_nanos) = Common.split_ns timeout_ns in
    Result.Ok {
      id = fresh_id ();
      timeout_ns;
      timeout_secs;
      timeout_nanos;
      repeat;
    }

let after_ns = make ~repeat:false

let every_ns = make ~repeat:true

let timeout_ns = fun timer -> timer.timeout_ns

let repeats = fun timer -> timer.repeat

let timeout_parts = fun timer -> (timer.timeout_secs, timer.timeout_nanos)

let to_source = fun timer ->
  let module Source = struct
    type nonrec t = t

    let register = fun timer selector token _interest ->
      Async.Adapter.Selector.register_timer
        selector
        ~timer_id:timer.id
        ~token
        ~timeout_parts:(timeout_parts timer)
        ~repeat:timer.repeat

    let reregister = fun timer selector token _interest ->
      Async.Adapter.Selector.reregister_timer
        selector
        ~timer_id:timer.id
        ~token
        ~timeout_parts:(timeout_parts timer)
        ~repeat:timer.repeat

    let deregister = fun timer selector ->
      Async.Adapter.Selector.deregister_timer selector ~timer_id:timer.id
  end in
  Async.Source.make (module Source) timer
