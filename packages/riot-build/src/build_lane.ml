open Std
open Std.Result.Syntax

type error =
  | BuildUnitPlanningFailed of Build_unit_plan.error
  | Failure of string

type unresolved

type locked

type build_plan = {
  package_names: Riot_model.Package_name.t list;
  scope: Resolved_build.scope;
  build_unit_plan: Build_unit_plan.t;
}

type 'stage t = {
  target: Riot_model.Target.t;
  workspace: Riot_model.Workspace.t;
  package_names: Riot_model.Package_name.t list;
  scope: Resolved_build.scope;
  profile_name: string;
  session_id: Riot_model.Session_id.t;
  host: Riot_model.Target.t;
  build_ctx: Riot_model.Build_ctx.t;
  toolchain: Riot_toolchain.t;
  store: Riot_store.Store.t;
  lock: Build_lock.t;
  build_unit_plan: Build_unit_plan.t;
}

let sort_unique_packages = fun package_names ->
  package_names
  |> List.unique ~compare:Riot_model.Package_name.compare
  |> List.sort ~compare:Riot_model.Package_name.compare

let make_build_ctx = fun ~host ~target ~toolchain ~session_id ~profile ~parallelism ->
  if Riot_model.Target.equal target host then
    Riot_model.Build_ctx.make ~session_id ~profile ~parallelism ()
  else
    let toolchain_root = Riot_toolchain.path toolchain in
    let cross_toolchain =
      Riot_toolchain.CrossCompilingToolchain.detect ~toolchain_root () ~target_triplet:target
    in
    Riot_model.Build_ctx.make
      ~session_id
      ~profile
      ~compilation_mode:(
        Riot_model.Build_ctx.Cross {
          target;
          sysroot = cross_toolchain.sysroot;
          bin_dir = cross_toolchain.bin_dir;
          bin_prefix = cross_toolchain.bin_prefix;
        }
      )
      ~parallelism
      ()

let build_unit_plan_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Build_unit_plan.MissingPackages { missing } ->
      let format_missing = fun __tmp1 ->
        match __tmp1 with
        | Riot_planner.Build_unit_graph.Root package ->
            "root:" ^ Riot_model.Package_name.to_string package
        | Dependency { package; dependency } ->
            "dependency:"
            ^ Riot_model.Package_name.to_string package
            ^ "->"
            ^ Riot_model.Package_name.to_string dependency
      in
      "missing build unit packages: " ^ String.concat "; " (List.map missing ~fn:format_missing)
  | CycleDetected { cycle } ->
      "build unit cycle detected: "
      ^ String.concat " -> " (List.map cycle ~fn:Riot_planner.Build_unit.key_to_string)

let error_message = fun __tmp1 ->
  match __tmp1 with
  | BuildUnitPlanningFailed error -> build_unit_plan_error_to_string error
  | Failure reason -> reason

let emit_phase = fun context phase -> Build_context.emit_phase context phase

let plan_build_units: Build_context.t -> Resolved_build.t -> (build_plan, error) result = fun
  context spec ->
  let package_names =
    Resolved_build.package_names spec
    |> sort_unique_packages
  in
  let scope = Resolved_build.scope spec in
  let plan_started_at = Time.Instant.now () in
  let* build_unit_plan =
    Build_unit_plan.create context spec
    |> Result.map_err ~fn:(fun error -> BuildUnitPlanningFailed error)
  in
  let plan_completed_at = Time.Instant.now () in
  emit_phase
    context
    (Event.BuildUnitPlanCreated {
      unit_count = List.length (Build_unit_plan.units build_unit_plan);
      planned_at = plan_completed_at;
      duration = Time.Instant.duration_since ~earlier:plan_started_at plan_completed_at;
    });
  Ok { package_names; scope; build_unit_plan }

let clone_build_plan = fun (plan: build_plan) -> plan

let release_on_error: 'value. Build_lock.t -> ('value, error) result -> ('value, error) result = fun
  lock result ->
  match result with
  | Ok value -> Ok value
  | Error _ as error ->
      Build_lock.release lock;
      error

let prepare:
  Build_context.t ->
  build_plan ->
  target:Riot_model.Target.t ->
  toolchain:Riot_toolchain.t ->
  (locked t, error) result = fun context plan ~target ~toolchain ->
  let workspace = context.workspace in
  let package_names = plan.package_names in
  let scope = plan.scope in
  let profile = context.profile in
  let session_id = context.session_id in
  let host = context.host in
  let lane_started_at = Time.Instant.now () in
  emit_phase context (Event.BuildLanePreparationStarted { target; started_at = lane_started_at });
  let lock_started_at = Time.Instant.now () in
  let* lock =
    Build_lock.wait
      ~on_waiting:(fun lock_path ->
        Build_context.emit_phase
          context
          (Event.BuildLockWaiting { lock_path }))
      ~target_dir_root:workspace.target_dir_root
      ~profile:profile.name
      ~target
    |> Result.map_err ~fn:(fun exn -> Failure (Exception.to_string exn))
  in
  let lock_acquired_at = Time.Instant.now () in
  emit_phase
    context
    (Event.BuildLaneLockAcquired {
      target;
      acquired_at = lock_acquired_at;
      duration = Time.Instant.duration_since ~earlier:lock_started_at lock_acquired_at;
    });
  let lane =
    try
      let toolchain_started_at = Time.Instant.now () in
      let* lane_toolchain =
        if Riot_model.Target.equal target host then
          Ok toolchain
        else
          Riot_toolchain.init_for_target ~config:context.toolchain_config ~target
          |> Result.map_err
            ~fn:(fun reason ->
              Failure ("failed to initialize toolchain for target "
              ^ Riot_model.Target.to_string target
              ^ ": "
              ^ reason))
      in
      let toolchain_initialized_at = Time.Instant.now () in
      emit_phase
        context
        (Event.BuildLaneToolchainInitialized {
          target;
          initialized_at = toolchain_initialized_at;
          duration = Time.Instant.duration_since
            ~earlier:toolchain_started_at
            toolchain_initialized_at;
        });
      let build_ctx =
        make_build_ctx
          ~host
          ~target
          ~toolchain:lane_toolchain
          ~session_id
          ~profile
          ~parallelism:context.parallelism
      in
      let store_started_at = Time.Instant.now () in
      let store = Riot_store.Store.create_for_lane ~workspace ~profile:profile.name ~target in
      let store_created_at = Time.Instant.now () in
      emit_phase
        context
        (Event.BuildLaneStoreCreated {
          target;
          created_at = store_created_at;
          duration = Time.Instant.duration_since ~earlier:store_started_at store_created_at;
        });
      let lane_prepared_at = Time.Instant.now () in
      emit_phase
        context
        (Event.BuildLanePreparationFinished {
          target;
          completed_at = lane_prepared_at;
          duration = Time.Instant.duration_since ~earlier:lane_started_at lane_prepared_at;
        });
      Ok {
        target;
        workspace;
        package_names;
        scope;
        profile_name = profile.name;
        session_id;
        host;
        build_ctx;
        toolchain = lane_toolchain;
        store;
        lock;
        build_unit_plan = plan.build_unit_plan;
      }
    with
    | exn -> Error (Failure (Exception.to_string exn))
  in
  release_on_error lock lane

let target = fun (lane: 'a t) -> lane.target

let workspace = fun (lane: 'a t) -> lane.workspace

let package_names = fun (lane: 'a t) -> lane.package_names

let scope = fun (lane: 'a t) -> lane.scope

let profile_name = fun (lane: 'a t) -> lane.profile_name

let session_id = fun (lane: 'a t) -> lane.session_id

let host = fun (lane: 'a t) -> lane.host

let build_ctx = fun (lane: 'a t) -> lane.build_ctx

let toolchain = fun (lane: 'a t) -> lane.toolchain

let store = fun (lane: 'a t) -> lane.store

let build_unit_plan = fun (lane: 'a t) -> lane.build_unit_plan

let build_unit_graph = fun (lane: 'a t) -> Build_unit_plan.graph lane.build_unit_plan

let build_units = fun (lane: 'a t) ->
  Build_unit_plan.units lane.build_unit_plan
  |> List.filter
    ~fn:(fun (unit: Riot_planner.Build_unit.t) ->
      Riot_model.Target.equal
        (Riot_planner.Build_unit.target unit)
        lane.target)

let build_unit_keys = fun lane ->
  build_units lane
  |> List.map ~fn:Riot_planner.Build_unit.key

let build_unit = fun lane key ->
  Riot_planner.Build_unit_graph.find (build_unit_graph lane) key
  |> Option.map ~fn:Riot_planner.Build_unit_graph.node_value

let build_unit_dependency_keys = fun lane key ->
  Riot_planner.Build_unit_graph.dependencies (build_unit_graph lane) key
  |> List.filter
    ~fn:(fun (key: Riot_planner.Build_unit.key) -> Riot_model.Target.equal key.target lane.target)

let release: locked t -> unit = fun lane -> Build_lock.release lane.lock
