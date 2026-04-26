open Std
open Std.Result.Syntax

type error =
  | InvalidRequestedParallelism of int

type public_event =
  | BuildingTarget of {
      target: Riot_model.Target.t;
      host: bool;
    }
  | CacheGc of Riot_store.Cache_gc.event
  | Phase of Event.runtime_phase

type Std.Telemetry.event +=
  | Public_build_event of {
      session_id: Riot_model.Session_id.t;
      event: public_event;
    }
  | Flush_build_events of {
      session_id: Riot_model.Session_id.t;
      reply_to: Pid.t;
      request_id: int;
    }

type Message.t +=
  | Build_events_flushed of { request_id: int }

type t = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

let no_event: Event.t -> unit = fun _ -> ()

let flush_request_counter = Sync.Atomic.make 0

let next_flush_request_id = fun () -> Sync.Atomic.fetch_and_add flush_request_counter 1 + 1

let same_session = fun left right ->
  String.equal (Riot_model.Session_id.to_string left) (Riot_model.Session_id.to_string right)

let emit_public_event = fun context event ->
  Std.Telemetry.emit (Public_build_event { session_id = context.session_id; event })

let emit_phase = fun context phase -> emit_public_event context (Phase phase)

let emit_building_target = fun context ~target ~host ->
  emit_public_event context (BuildingTarget { target; host })

let emit_cache_gc = fun context event -> emit_public_event context (CacheGc event)

let forward_telemetry_event = fun context (event: Std.Telemetry.event) ->
  match event with
  | Public_build_event { session_id; event } when same_session session_id context.session_id -> (
      match event with
      | BuildingTarget { target; host } -> context.on_event (Event.BuildingTarget { target; host })
      | CacheGc event -> context.on_event (Event.CacheGc event)
      | Phase phase -> context.on_event (Event.Phase phase)
    )
  | Flush_build_events { session_id; reply_to; request_id } when same_session
    session_id
    context.session_id -> send reply_to (Build_events_flushed { request_id })
  | _ -> (
      match Telemetry_events.event_session_id event with
      | Some session_id when same_session session_id context.session_id ->
          context.on_event (Event.Telemetry event)
      | Some _
      | None -> ()
    )

let flush_events = fun context ->
  let request_id = next_flush_request_id () in
  Std.Telemetry.emit
    (Flush_build_events { session_id = context.session_id; reply_to = self (); request_id });
  let selector message =
    match message with
    | Build_events_flushed { request_id = got } when Int.equal got request_id -> `select ()
    | _ -> `skip
  in
  receive ~selector ()

let requested_parallelism = fun request ->
  match Request.Internal.requested_parallelism request with
  | Some requested when requested < 1 -> Error (InvalidRequestedParallelism requested)
  | Some requested -> Ok (Int.min Thread.available_parallelism requested)
  | None -> Ok Thread.available_parallelism

let make = fun ?(on_event = no_event) request ->
  let workspace = Request.Internal.workspace request in
  let* parallelism = requested_parallelism request in
  Ok {
    session_id = Riot_model.Session_id.make ();
    workspace;
    profile = Request.Internal.profile request;
    host = Riot_model.Target.current;
    toolchain_config = Riot_model.Toolchain_config.from_root
      ~root:workspace.Riot_model.Workspace.root;
    parallelism = Int.max 1 parallelism;
    on_event;
  }
