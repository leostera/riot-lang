open Std
open Std.Result.Syntax

type t = {
  workspace: Riot_model.Workspace.t;
  catalog: Package_catalog.t;
  store: Riot_store.Store.t;
  session_id: Riot_model.Session_id.t;
  parallelism: int;
  toolchains: Toolchain_service.t;
}

type input = {
  build: Goal.build_package;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  toolchain: Riot_toolchain.t;
  build_ctx: Riot_model.Build_ctx.t;
  package_hash: Crypto.hash;
}

type artifact_hit = {
  build: Goal.build_package;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  artifact: Riot_store.Artifact.t;
}

let create = fun ~workspace ~catalog ~store ~session_id ~parallelism ~toolchains () ->
  {
    workspace;
    catalog;
    store;
    session_id;
    parallelism;
    toolchains;
  }

let apply_package_profile = fun ~(package:Riot_model.Package.t) ~build_ctx profile ->
  let profile = Riot_model.Profile.apply_overrides profile package.compiler.profile_overrides in
  let target_platform = Riot_model.Build_ctx.target_platform_name build_ctx in
  List.find
    package.compiler.target_overrides
    ~fn:(fun (target, _) -> String.equal target target_platform)
  |> Option.and_then
    ~fn:(fun (_, (target_override: Riot_model.Package.target_override)) ->
      target_override.profile_override)
  |> Option.map ~fn:(fun override -> Riot_model.Profile.apply_override profile override)
  |> Option.unwrap_or ~default:profile

let build_ctx = fun t ~(profile:Riot_model.Profile.t) ~target ->
  let host = Riot_toolchain.get_host_triple () in
  let compilation_mode =
    if Riot_model.Target.equal host target then
      Riot_model.Build_ctx.HostOnly
    else
      Riot_model.Build_ctx.Cross {
        target;
        sysroot = None;
        bin_dir = None;
        bin_prefix = "";
      }
  in
  Riot_model.Build_ctx.make
    ~session_id:t.session_id
    ~profile
    ~compilation_mode
    ~parallelism:t.parallelism
    ()

let package_input_hash = fun
  t ~(package:Riot_model.Package.t) ~profile ~build_ctx ~toolchain ~depset ->
  Riot_planner.Package_planner.compute_input_hash
    ~planner_version:"riot-build2-package-input:v1"
    ~package
    ~depset
    ~workspace:t.workspace
    ~profile
    ~build_ctx
    ~toolchain
    ()

let dependency_builds = fun t (build: Goal.build_package) ->
  let* dependencies =
    Package_catalog.dependency_names_for_scope
      t.catalog
      ~scope:(Goal.dependency_scope build.scope)
      build.package
  in
  Ok (
    List.map
      dependencies
      ~fn:(fun package ->
        Goal.{
          package;
          scope = build.scope;
          profile = build.profile;
          target = build.target;
        })
  )

let resolve = fun ?(depset = []) t (build: Goal.build_package) ->
  let* package =
    Package_catalog.realize t.catalog ~intent:(Goal.realization_intent build.scope) build.package
  in
  let toolchain = Toolchain_service.expected t.toolchains build.target in
  let base_ctx = build_ctx t ~profile:build.profile ~target:build.target in
  let profile = apply_package_profile ~package ~build_ctx:base_ctx build.profile in
  let build_ctx = build_ctx t ~profile ~target:build.target in
  let package_hash = package_input_hash t ~package ~profile ~build_ctx ~toolchain ~depset in
  Ok {
    build;
    package;
    profile;
    target = build.target;
    toolchain;
    build_ctx;
    package_hash;
  }

let missing_dependency_artifact = fun (build: Goal.build_package) ->
  Error.ExecutorInvariantViolated {
    message = "package dependency "
    ^ Riot_model.Package_name.to_string build.Goal.package
    ^ " was planned as complete but its package artifact was not in riot-store";
  }

let rec dependency_of_build = fun t path (build: Goal.build_package) ->
  if List.any path ~fn:(fun seen -> seen = build) then
    Error (Error.ExecutorInvariantViolated {
      message = "cyclic package dependency while computing depset for "
      ^ Riot_model.Package_name.to_string build.Goal.package;
    })
  else
    let path = build :: path in
    let* dependency_builds = dependency_builds t build in
    let* child_depset = depset_of_builds t path dependency_builds in
    let* input = resolve ~depset:child_depset t build in
    match Riot_store.Store.get_package_metadata t.store input.package_hash with
    | None -> Error (missing_dependency_artifact build)
    | Some artifact ->
        Ok Riot_planner.Dependency.{
          package = input.package;
          artifact_dir = Riot_store.Store.hash_dir_of t.store artifact.input_hash;
          depset = child_depset;
          input_hash = artifact.input_hash;
          output_hash = artifact.output_hash;
        }

and depset_of_builds = fun t path (builds: Goal.build_package list) ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | build :: rest ->
        let* dependency = dependency_of_build t path build in
        loop (dependency :: acc) rest
  in
  loop [] builds

let depset = fun t (build: Goal.build_package) ->
  let* dependency_builds = dependency_builds t build in
  depset_of_builds t [ build ] dependency_builds

let cached_artifact = fun t build ->
  let* depset = depset t build in
  let* input = resolve ~depset t build in
  match Riot_store.Store.get_package_metadata t.store input.package_hash with
  | None -> Ok None
  | Some artifact ->
      Ok (
        Some {
          build = input.build;
          package = input.package;
          profile = input.profile;
          target = input.target;
          artifact;
        }
      )
