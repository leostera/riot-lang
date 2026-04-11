open Std

type t = {
  capture_traces: bool;
  on_event: (Event.t -> unit) option;
}

let default = {
  capture_traces = true;
  on_event = None;
}

let with_capture_traces = fun config ~capture_traces -> { config with capture_traces }

let with_on_event = fun config ~on_event -> { config with on_event = Some on_event }

let without_on_event = fun config -> { config with on_event = None }

let monotonic_now_us = fun () -> Int64.(to_int (div (Kernel.Time.monotonic_time_nanos ()) 1_000L))

let emit_event = fun config build_event ->
  match config.on_event with
  | None -> ()
  | Some on_event ->
      let instant_us = monotonic_now_us () in
      on_event (build_event ~instant_us)
