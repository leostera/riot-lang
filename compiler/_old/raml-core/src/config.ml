open Std
open Std.Result.Syntax

type t = {
  on_event: (Event.t -> unit) option;
  host: Target.t;
  target: Target.t;
  content_store: Contentstore.t option;
}

let default = {
  on_event = None;
  host = Target.unknown_unknown_unknown;
  target = Target.js_unknown_ecma;
  content_store = None;
}

let validate_target = fun ~name (target: Target.t) ->
  if String.equal target.architecture "" then
    Error (name ^ " target triple must include a non-empty architecture")
  else if String.equal target.vendor "" then
    Error (name ^ " target triple must include a non-empty vendor")
  else if String.equal target.system "" then
    Error (name ^ " target triple must include a non-empty system")
  else
    Ok ()

let validate = fun config ->
  let* () = validate_target ~name:"host" config.host in
  validate_target ~name:"target" config.target

let make = fun ?on_event ?(host = default.host) ?(target = default.target) ?content_store () ->
  {
    on_event;
    host;
    target;
    content_store;
  }

let with_on_event = fun config ~on_event -> { config with on_event = Some on_event }

let without_on_event = fun config -> { config with on_event = None }

let with_host = fun config ~host -> { config with host }

let with_target = fun config ~target -> { config with target }

let with_targeting = fun config ~host ~target -> { config with host; target }

let with_content_store = fun config ~content_store ->
  { config with content_store = Some content_store }

let without_content_store = fun config -> { config with content_store = None }

let host = fun config -> config.host

let target = fun config -> config.target

let content_store = fun config -> config.content_store

let select_backend = fun config -> Target.select_backend ~host:config.host ~target:config.target

let monotonic_now_us = fun () ->
  let instant = Kernel.Time.Monotonic.now () |> Result.expect ~msg:"failed to read monotonic clock" in
  let secs, nanos = Kernel.Time.Monotonic.to_parts instant in
  Int64.(to_int (add (mul (from_int secs) 1_000_000L) (div (from_int nanos) 1_000L)))

let emit_event = fun config build_event ->
  match config.on_event with
  | None -> ()
  | Some on_event -> on_event { Event.instant_us = monotonic_now_us (); kind = build_event () }
