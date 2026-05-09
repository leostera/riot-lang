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
  build: Package_work.build_library;
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  toolchain: Riot_toolchain.t;
  build_ctx: Riot_model.Build_ctx.t;
  package_hash: Crypto.hash;
}

type artifact_hit = {
  build: Package_work.build_library;
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

let package_input_hash = fun t ~(package:Riot_model.Package.t) ~profile ~build_ctx ~toolchain ->
  Riot_planner.Package_planner.compute_input_hash
    ~planner_version:"riot-build2-package-input:v1"
    ~package
    ~depset:[]
    ~workspace:t.workspace
    ~profile
    ~build_ctx
    ~toolchain
    ()

let resolve = fun t (build: Package_work.build_library) ->
  let* package = Package_catalog.realize t.catalog ~intent:Riot_model.Package.Runtime build.package in
  match Toolchain_service.find t.toolchains build.target with
  | None ->
      Error (Error.ToolchainFailed {
        target = build.target;
        reason = "toolchain was not ready before package planning";
      })
  | Some toolchain ->
      let base_ctx = build_ctx t ~profile:build.profile ~target:build.target in
      let profile = apply_package_profile ~package ~build_ctx:base_ctx build.profile in
      let build_ctx = build_ctx t ~profile ~target:build.target in
      let package_hash = package_input_hash t ~package ~profile ~build_ctx ~toolchain in
      Ok {
        build;
        package;
        profile;
        target = build.target;
        toolchain;
        build_ctx;
        package_hash;
      }

let cached_artifact = fun t build ->
  let* input = resolve t build in
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
