open Std
open Std.Result.Syntax

type error =
  | InvalidRequestedParallelism of int

type t = {
  session_id: Riot_model.Session_id.t;
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  targets: Riot_model.Target.Set.t;
  scope: Build_spec.scope;
  profile: Riot_model.Profile.t;
  host: Riot_model.Target.t;
  toolchain_config: Riot_model.Toolchain_config.t;
  parallelism: int;
  on_event: Event.t -> unit;
}

let no_event: Event.t -> unit = fun _ -> ()

let requested_parallelism = fun spec ->
  match Build_spec.requested_parallelism spec with
  | Some requested when requested < 1 -> Error (InvalidRequestedParallelism requested)
  | Some requested -> Ok (Int.min Thread.available_parallelism requested)
  | None -> Ok Thread.available_parallelism

let make = fun ?(on_event = no_event) spec ->
  let workspace = Build_spec.workspace spec in
  let* parallelism = requested_parallelism spec in
  Ok {
    session_id = Riot_model.Session_id.make ();
    workspace;
    package_names = Build_spec.package_names spec;
    targets = Build_spec.targets spec;
    scope = Build_spec.scope spec;
    profile = Build_spec.profile spec;
    host = Riot_model.Target.current;
    toolchain_config = Riot_model.Toolchain_config.from_root ~root:workspace.Riot_model.Workspace.root;
    parallelism = Int.max 1 parallelism;
    on_event;
  }
