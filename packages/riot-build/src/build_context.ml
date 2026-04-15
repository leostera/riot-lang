open Std
open Std.Result.Syntax

type error =
  | InvalidRequestedParallelism of int

type t = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: Resolved_build.scope;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

let no_event: Event.t -> unit = fun _ -> ()

let requested_parallelism = fun spec ->
  match Resolved_build.requested_parallelism spec with
  | Some requested when requested < 1 -> Error (InvalidRequestedParallelism requested)
  | Some requested -> Ok (Int.min Thread.available_parallelism requested)
  | None -> Ok Thread.available_parallelism

let make = fun ?(on_event = no_event) spec ->
  let workspace = Resolved_build.workspace spec in
  let* parallelism = requested_parallelism spec in
  Ok {
    session_id = Riot_model.Session_id.make ();
    workspace;
    package_names = Resolved_build.package_names spec;
    targets = Resolved_build.targets spec;
    scope = Resolved_build.scope spec;
    profile = Resolved_build.profile spec;
    host = Riot_model.Target.current;
    toolchain_config = Riot_model.Toolchain_config.from_root ~root:workspace.Riot_model.Workspace.root;
    parallelism = Int.max 1 parallelism;
    on_event;
  }
