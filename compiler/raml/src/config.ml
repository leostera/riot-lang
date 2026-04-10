open Std

type t = {
  on_event: (Event.t -> unit) option;
  host: Target.t;
  target: Target.t;
}

let default = {
  on_event = None;
  host = Target.unknown_unknown_unknown;
  target = Target.js_unknown_ecma
}

let make = fun ?on_event ?(host = default.host) ?(target = default.target) () ->
  { on_event; host; target }

let with_on_event = fun config ~on_event -> { config with on_event = Some on_event }

let without_on_event = fun config -> { config with on_event = None }

let with_host = fun config ~host -> { config with host }

let with_target = fun config ~target -> { config with target }

let with_targeting = fun config ~host ~target -> { config with host; target }

let monotonic_now_us = fun () -> Int64.(to_int (div (Kernel.Time.monotonic_time_nanos ()) 1_000L))

let emit_event = fun config build_event ->
  match config.on_event with
  | None -> ()
  | Some on_event -> on_event { Event.instant_us = monotonic_now_us (); kind = build_event () }
