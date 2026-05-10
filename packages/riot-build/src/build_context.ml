open Std
open Std.Result.Syntax

type error =
  | InvalidRequestedParallelism of int

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

let emit_kind = fun ?level context kind ->
  context.on_event
    (Event.create ~session_id:context.session_id ?level kind)

let emit_build_event = fun context event -> emit_kind context (Riot_model.Event.Build event)

let emit_phase = fun context phase -> emit_build_event context (Riot_model.Event.BuildPhase phase)

let emit_building_target = fun context ~target ~host ->
  emit_build_event
    context
    (Riot_model.Event.BuildTargetBuilding { target; host })

let emit_cache_gc = fun context event ->
  emit_kind
    context
    (Riot_model.Event.Cache (Event.cache_gc_event_kind event))

let flush_events = fun (_: t) -> ()

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
