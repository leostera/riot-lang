open Std

type t = {
  capture_traces: bool;
  on_event: (Event.t -> unit) option;
}

let default = { capture_traces = true; on_event = None }

let with_capture_traces = fun config ~capture_traces -> { config with capture_traces }

let with_on_event = fun config ~on_event -> { config with on_event = Some on_event }

let without_on_event = fun config -> { config with on_event = None }

let monotonic_now_us = fun () ->
  let instant = Kernel.Time.Monotonic.now () |> Result.expect ~msg:"failed to read monotonic clock" in
  let secs, nanos = Kernel.Time.Monotonic.to_parts instant in
  Int64.(to_int (add (mul (of_int secs) 1_000_000L) (div (of_int nanos) 1_000L)))

let emit_event = fun config build_event ->
  match config.on_event with
  | None -> ()
  | Some on_event ->
      let instant_us = monotonic_now_us () in
      on_event (build_event ~instant_us)
