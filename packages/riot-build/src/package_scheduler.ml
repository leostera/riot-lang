open Std
open Std.Collections

type package_work = {
  lane: Build_lane.locked Build_lane.t;
  package_key: Riot_model.Package.key;
}

type package_state =
  | AwaitingPlan
  | AwaitingExecution of Package_builder.execution_plan
  | Finalized of Package_builder.detailed_result

type mutation =
  | Apply_graph_update of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      package: Riot_model.Package.t;
      graph_update: Package_builder.graph_update option;
    }
  | Set_package_state of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      state: package_state;
    }

type planning_output =
  | PlanningDeferred of package_work
  | PlanningRequiresExecution of {
      lane: Build_lane.locked Build_lane.t;
      execution_plan: Package_builder.execution_plan;
    }
  | PlanningFinalized of {
      lane: Build_lane.locked Build_lane.t;
      detailed_result: Package_builder.detailed_result;
    }

type execution_output =
  | ExecutionFinalized of {
      lane: Build_lane.locked Build_lane.t;
      detailed_result: Package_builder.detailed_result;
    }

type error = {
  lane: Build_lane.locked Build_lane.t;
  reason: string;
}

type completion = {
  target: Riot_model.Target.t;
  result_count: int;
  had_partial_failure: bool;
}

type summary = {
  completions: completion list;
  lane_results: Lane_result.t list;
  errors: error list;
  had_failure: bool;
}

type event =
  | PlanningRoundStarted of {
      lane_count: int;
      package_count: int;
    }
  | PlanningRoundFinished of {
      lane_count: int;
      package_count: int;
      deferred_count: int;
      execution_required_count: int;
      finalized_count: int;
      cached_count: int;
      skipped_count: int;
      failed_count: int;
      error_count: int;
    }
  | ExecutionRoundStarted of {
      lane_count: int;
      package_count: int;
    }
  | ExecutionRoundFinished of {
      lane_count: int;
      package_count: int;
      finalized_count: int;
      built_count: int;
      failed_count: int;
      error_count: int;
    }

let lane_id = fun lane -> Riot_model.Target.to_string (Build_lane.target lane)

let package_key_id = fun package_key -> Riot_model.Package.key_to_string package_key

let package_state_key = fun lane package_key ->
  lane_id lane ^ "#" ^ package_key_id package_key

let dependency_nodes = fun lane package_key ->
  match Riot_planner.Package_graph.get_node_by_key (Build_lane.package_graph lane) package_key with
  | None -> []
  | Some node ->
      Riot_planner.Package_graph.get_dependencies_for_node (Build_lane.package_graph lane) node

let dependency_keys = fun lane package_key ->
  dependency_nodes lane package_key
  |> List.map ~fn:Riot_planner.Package_graph.get_key

let skipped_reason = fun failed_packages ->
  let failed_names =
    failed_packages
    |> List.map ~fn:(fun (package: Riot_model.Package.t) ->
      Riot_model.Package_name.to_string package.name)
  in
  "needs " ^ String.concat ", " failed_names

let dependency_failed_state = function
  | Finalized {
    result = {
      status =
        Package_builder.Failed _
        | Package_builder.Skipped _;
      package;
      _;
    };
    _;
  } -> Some package
  | Finalized {
    result = {
      status =
        Package_builder.Built _
        | Package_builder.Cached _;
      _;
    };
    _;
  }
  | AwaitingPlan
  | AwaitingExecution _ -> None

let apply_graph_update = fun lane package_key package graph_update ->
  match Riot_planner.Package_graph.get_node_by_key (Build_lane.package_graph lane) package_key with
  | None -> ()
  | Some node ->
      let scope = Riot_planner.Package_graph.get_scope node.value in
      match graph_update with
      | None -> ()
      | Some (Package_builder.Planned_package { hash; module_graph; action_graph }) ->
          node.value <- Riot_planner.Package_graph.Planned {
            package;
            scope;
            module_graph;
            action_graph;
            hash;
          }
      | Some (Package_builder.Cached_package { hash; artifact; depset; exports }) ->
          node.value <- Riot_planner.Package_graph.Cached {
            package;
            scope;
            hash;
            artifact;
            depset;
            exports;
          }
      | Some (Package_builder.Built_package { hash; artifact; depset; module_graph; action_graph; status }) ->
          node.value <- Riot_planner.Package_graph.Built {
            package;
            scope;
            module_graph;
            action_graph;
            hash;
            artifact;
            status;
            depset;
          }
      | Some (Package_builder.Failed_package { hash = Some hash; error }) ->
          node.value <- Riot_planner.Package_graph.Failed {
            package;
            scope;
            hash;
            error;
          }
      | Some (Package_builder.Failed_package { hash = None; _ }) -> ()
      | Some (Package_builder.Skipped_package { reason }) ->
          node.value <- Riot_planner.Package_graph.Skipped {
            package;
            scope;
            reason;
          }

let remember_package_state = fun package_states lane package_key state ->
  let _ = HashMap.insert
    package_states
    ~key:(package_state_key lane package_key)
    ~value:state
  in
  ()

let get_package_state = fun package_states lane package_key ->
  HashMap.get package_states ~key:(package_state_key lane package_key)

let record_graph_update = fun handle lane ~package_key ~package graph_update ->
  Graph_scheduler.Handle.record
    handle
    (Apply_graph_update {
      lane;
      package_key;
      package;
      graph_update;
    })

let record_package_state = fun handle lane package_key state ->
  Graph_scheduler.Handle.record
    handle
    (Set_package_state {
      lane;
      package_key;
      state;
    })

let awaiting_plan_work = fun lanes package_states ->
  lanes
  |> List.flat_map ~fn:(fun lane ->
    Build_lane.package_keys lane
    |> List.filter_map ~fn:(fun package_key ->
      match get_package_state package_states lane package_key with
      | Some AwaitingPlan -> Some { lane; package_key }
      | Some (AwaitingExecution _)
      | Some (Finalized _)
      | None -> None))

let awaiting_execution_work = fun lanes package_states ->
  lanes
  |> List.flat_map ~fn:(fun lane ->
    Build_lane.package_keys lane
    |> List.filter_map ~fn:(fun package_key ->
      match get_package_state package_states lane package_key with
      | Some (AwaitingExecution execution_plan) ->
          Some {
            lane;
            package_key = execution_plan.package_key;
          }
      | Some AwaitingPlan
      | Some (Finalized _)
      | None -> None))

let pending_dependencies = fun package_states lane package_key ->
  dependency_keys lane package_key
  |> List.filter ~fn:(fun dependency_key ->
    match get_package_state package_states lane dependency_key with
    | Some AwaitingPlan
    | Some (AwaitingExecution _) -> true
    | Some (Finalized _)
    | None -> false)

let skipped_result_if_failed_dependencies =
  fun
    package_states
    lane
    package_key
    (package_node: Riot_planner.Package_graph.package_node Graph.SimpleGraph.node) ->
  let failed_dependencies =
    dependency_keys lane package_key
    |> List.filter_map ~fn:(fun dependency_key ->
      match get_package_state package_states lane dependency_key with
      | Some state -> dependency_failed_state state
      | None -> None)
  in
  if failed_dependencies = [] then
    None
  else
    let package = Riot_planner.Package_graph.get_package package_node.value in
    let reason = skipped_reason failed_dependencies in
    Some Package_builder.{
      result = {
        package_key;
        package;
        status = Skipped { reason };
        ocamlc_warnings = [];
        duration = Time.Duration.zero;
      };
      graph_update = Some (Skipped_package { reason });
    }

let finalize_result = fun graph lane (detailed_result: Package_builder.detailed_result) ->
  record_graph_update
    graph
    lane
    ~package_key:detailed_result.result.package_key
    ~package:detailed_result.result.package
    detailed_result.graph_update;
  record_package_state
    graph
    lane
    detailed_result.result.package_key
    (Finalized detailed_result)

let plan_package_work =
  fun
    ~package_states
    ~graph
    ({ lane; package_key } as work: package_work) ->
    match Riot_planner.Package_graph.get_node_by_key (Build_lane.package_graph lane) package_key with
    | None ->
        Error {
          lane;
          reason = "package graph missing node for " ^ package_key_id work.package_key;
        }
    | Some (package_node: Riot_planner.Package_graph.package_node Graph.SimpleGraph.node) ->
        let waiting_on_dependencies = pending_dependencies package_states lane package_key in
        if waiting_on_dependencies != [] then
          Ok (PlanningDeferred work)
        else
          (
            match skipped_result_if_failed_dependencies package_states lane package_key package_node with
            | Some detailed_result ->
                finalize_result graph lane detailed_result;
                Ok (PlanningFinalized { lane; detailed_result })
            | None ->
                let package = Riot_planner.Package_graph.get_package package_node.value in
                match Package_builder.plan_detailed
                  ~workspace:(Build_lane.workspace lane)
                  ~toolchain:(Build_lane.toolchain lane)
                  ~store:(Build_lane.store lane)
                  ~package_graph:(Build_lane.package_graph lane)
                  ~package_key
                  ~package
                  ~build_ctx:(Build_lane.build_ctx lane) with
                | Package_builder.Final_result detailed_result ->
                    finalize_result graph lane detailed_result;
                    Ok (PlanningFinalized { lane; detailed_result })
                | Package_builder.Execution_required execution_plan ->
                    record_graph_update
                      graph
                      lane
                      ~package_key
                      ~package
                      (Some (Package_builder.planned_graph_update execution_plan));
                    record_package_state
                      graph
                      lane
                      package_key
                      (AwaitingExecution execution_plan);
                    Ok (PlanningRequiresExecution { lane; execution_plan })
          )

let execute_package_work =
  fun
    ~package_states
    ~graph
    ({ lane; package_key }: package_work) ->
    match get_package_state package_states lane package_key with
    | Some (AwaitingExecution execution_plan) ->
        let detailed_result =
          Package_builder.execute_detailed
            ~workspace:(Build_lane.workspace lane)
            ~toolchain:(Build_lane.toolchain lane)
            ~store:(Build_lane.store lane)
            ~package_graph:(Build_lane.package_graph lane)
            ~execution_plan
            ~build_ctx:(Build_lane.build_ctx lane)
        in
        finalize_result graph lane detailed_result;
        Ok (ExecutionFinalized { lane; detailed_result })
    | Some AwaitingPlan ->
        Error {
          lane;
          reason = "package execution ran before planning for " ^ package_key_id package_key;
        }
    | Some (Finalized _) ->
        Error {
          lane;
          reason = "package execution reran after finalization for " ^ package_key_id package_key;
        }
    | None ->
        Error {
          lane;
          reason = "package state missing for execution " ^ package_key_id package_key;
        }

let make_graph = fun ~package_states ~work_items ~dependency_selector ->
  let graph =
    Graph_scheduler.Graph.create
      ~apply_mutation:(fun _ mutation ->
        match mutation with
        | Apply_graph_update { lane; package_key; package; graph_update } ->
            apply_graph_update lane package_key package graph_update
        | Set_package_state { lane; package_key; state } ->
            remember_package_state package_states lane package_key state)
      ()
  in
  let node_ids: (string, Graph_scheduler.Node_id.t) HashMap.t = HashMap.create () in
  List.for_each work_items
    ~fn:(fun ({ lane; package_key } as work) ->
      let node_id = Graph_scheduler.Graph.add_node graph ~payload:work in
      let _ = HashMap.insert
        node_ids
        ~key:(package_state_key lane package_key)
        ~value:node_id
      in
      ());
  List.for_each work_items
    ~fn:(fun ({ lane; package_key } as work) ->
      let node_id =
        HashMap.get node_ids ~key:(package_state_key lane package_key)
        |> Option.expect ~msg:("missing graph node for " ^ package_state_key lane package_key)
      in
      dependency_selector work
      |> List.for_each ~fn:(fun dependency_key ->
        match HashMap.get node_ids ~key:(package_state_key lane dependency_key) with
        | Some dependency_node_id ->
            Graph_scheduler.Graph.add_dependency
              graph
              ~node:node_id
              ~depends_on:dependency_node_id
        | None -> ()));
  graph

let make_planning_graph = fun ~package_states lanes ->
  let work_items = awaiting_plan_work lanes package_states in
  let dependency_selector = fun ({ lane; package_key }: package_work) ->
    dependency_keys lane package_key
    |> List.filter ~fn:(fun dependency_key ->
      match get_package_state package_states lane dependency_key with
      | Some AwaitingPlan -> true
      | Some (AwaitingExecution _)
      | Some (Finalized _)
      | None -> false)
  in
  (work_items, make_graph ~package_states ~work_items ~dependency_selector)

let make_execution_graph = fun ~package_states lanes ->
  let work_items = awaiting_execution_work lanes package_states in
  let dependency_selector = fun (_: package_work) -> [] in
  (work_items, make_graph ~package_states ~work_items ~dependency_selector)

type planning_round_counts = {
  deferred_count: int;
  execution_required_count: int;
  finalized_count: int;
  cached_count: int;
  skipped_count: int;
  failed_count: int;
  error_count: int;
}

type execution_round_counts = {
  finalized_count: int;
  built_count: int;
  failed_count: int;
  error_count: int;
}

let summarize_planning_round = fun results ->
  List.fold_left results
    ~acc:{
      deferred_count = 0;
      execution_required_count = 0;
      finalized_count = 0;
      cached_count = 0;
      skipped_count = 0;
      failed_count = 0;
      error_count = 0;
    }
    ~fn:(fun counts (result: (package_work, planning_output, error) Graph_scheduler.node_result) ->
      match result.outcome with
      | Error _ -> { counts with error_count = counts.error_count + 1 }
      | Ok (PlanningDeferred _) -> { counts with deferred_count = counts.deferred_count + 1 }
      | Ok (PlanningRequiresExecution _) -> {
        counts
        with execution_required_count = counts.execution_required_count + 1
      }
      | Ok (PlanningFinalized { detailed_result; _ }) ->
          let counts = { counts with finalized_count = counts.finalized_count + 1 } in
          (match detailed_result.result.status with
          | Package_builder.Cached _ -> { counts with cached_count = counts.cached_count + 1 }
          | Package_builder.Skipped _ -> { counts with skipped_count = counts.skipped_count + 1 }
          | Package_builder.Failed _ -> { counts with failed_count = counts.failed_count + 1 }
          | Package_builder.Built _ -> counts))

let summarize_execution_round = fun results ->
  List.fold_left results
    ~acc:{
      finalized_count = 0;
      built_count = 0;
      failed_count = 0;
      error_count = 0;
    }
    ~fn:(fun counts (result: (package_work, execution_output, error) Graph_scheduler.node_result) ->
      match result.outcome with
      | Error _ -> { counts with error_count = counts.error_count + 1 }
      | Ok (ExecutionFinalized { detailed_result; _ }) ->
          let counts = { counts with finalized_count = counts.finalized_count + 1 } in
          (match detailed_result.result.status with
          | Package_builder.Built _ -> { counts with built_count = counts.built_count + 1 }
          | Package_builder.Failed _ -> { counts with failed_count = counts.failed_count + 1 }
          | Package_builder.Cached _
          | Package_builder.Skipped _ -> counts))

let run_planning_round = fun ~parallelism ~package_states ~on_event lanes ->
  let work_items, graph = make_planning_graph ~package_states lanes in
  if work_items = [] then
    []
  else (
    on_event (PlanningRoundStarted {
      lane_count = List.length lanes;
      package_count = List.length work_items;
    });
    let results =
      Graph_scheduler.run
      ~config:(Graph_scheduler.Run_config.make
        ~parallelism
        ~mode:Graph_scheduler.Run_config.Continue_on_failure
        ())
      ~on_event:(fun () -> ())
      ~graph
      ~execute:(fun ~graph ~node:_ ~payload ->
        plan_package_work ~package_states ~graph payload)
      |> fun results -> results.results
    in
    let counts = summarize_planning_round results in
    on_event (PlanningRoundFinished {
      lane_count = List.length lanes;
      package_count = List.length work_items;
      deferred_count = counts.deferred_count;
      execution_required_count = counts.execution_required_count;
      finalized_count = counts.finalized_count;
      cached_count = counts.cached_count;
      skipped_count = counts.skipped_count;
      failed_count = counts.failed_count;
      error_count = counts.error_count;
    });
    results
  )

let run_execution_round = fun ~parallelism ~package_states ~on_event lanes ->
  let work_items, graph = make_execution_graph ~package_states lanes in
  if work_items = [] then
    []
  else (
    on_event (ExecutionRoundStarted {
      lane_count = List.length lanes;
      package_count = List.length work_items;
    });
    let results =
      Graph_scheduler.run
      ~config:(Graph_scheduler.Run_config.make
        ~parallelism
        ~mode:Graph_scheduler.Run_config.Continue_on_failure
        ())
      ~on_event:(fun () -> ())
      ~graph
      ~execute:(fun ~graph ~node:_ ~payload ->
        execute_package_work ~package_states ~graph payload)
      |> fun results -> results.results
    in
    let counts = summarize_execution_round results in
    on_event (ExecutionRoundFinished {
      lane_count = List.length lanes;
      package_count = List.length work_items;
      finalized_count = counts.finalized_count;
      built_count = counts.built_count;
      failed_count = counts.failed_count;
      error_count = counts.error_count;
    });
    results
  )

let collect_errors = fun results ->
  List.filter_map results ~fn:(fun (result: (_, _, error) Graph_scheduler.node_result) ->
    match result.outcome with
    | Error err -> Some err
    | Ok _ -> None)

type scheduler_state = {
  package_states: (string, package_state) HashMap.t;
}

type pending_counts = {
  awaiting_plan: int;
  awaiting_execution: int;
  finalized: int;
}

let make_state = fun lanes ->
  let package_states: (string, package_state) HashMap.t = HashMap.create () in
  List.for_each lanes
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.for_each ~fn:(fun package_key ->
        remember_package_state package_states lane package_key AwaitingPlan));
  { package_states }

let pending_counts = fun lanes package_states ->
  lanes
  |> List.fold_left
    ~acc:{ awaiting_plan = 0; awaiting_execution = 0; finalized = 0; }
    ~fn:(fun counts lane ->
      Build_lane.package_keys lane
      |> List.fold_left ~acc:counts ~fn:(fun counts package_key ->
        match get_package_state package_states lane package_key with
        | Some AwaitingPlan -> { counts with awaiting_plan = counts.awaiting_plan + 1 }
        | Some (AwaitingExecution _) -> {
          counts
          with awaiting_execution = counts.awaiting_execution + 1
        }
        | Some (Finalized _) -> { counts with finalized = counts.finalized + 1 }
        | None -> counts))

let pending_counts_changed = fun left right ->
  left.awaiting_plan != right.awaiting_plan
  || left.awaiting_execution != right.awaiting_execution
  || left.finalized != right.finalized

let pending_descriptions = fun lanes package_states ->
  lanes
  |> List.flat_map ~fn:(fun lane ->
    Build_lane.package_keys lane
    |> List.filter_map ~fn:(fun package_key ->
      match get_package_state package_states lane package_key with
      | Some AwaitingPlan ->
          Some ("awaiting plan: " ^ package_key_id package_key)
      | Some (AwaitingExecution _) ->
          Some ("awaiting execution: " ^ package_key_id package_key)
      | Some (Finalized _)
      | None -> None))

let stalled_errors = fun lanes package_states ->
  let pending = pending_descriptions lanes package_states in
  if pending = [] then
    []
  else
    let reason =
      "package scheduler made no progress with pending work: "
      ^ String.concat ", " pending
    in
    List.map lanes ~fn:(fun lane -> { lane; reason })

let lane_result_of_states = fun package_states lane ->
  let package_results =
    Build_lane.package_keys lane
    |> List.filter_map ~fn:(fun package_key ->
      match get_package_state package_states lane package_key with
      | Some (Finalized detailed_result) -> Some detailed_result.result
      | Some AwaitingPlan
      | Some (AwaitingExecution _)
      | None -> None)
  in
  if package_results = [] then
    None
  else
    let had_partial_failure =
      List.any package_results ~fn:(fun (result: Package_builder.build_result) ->
        match result.status with
        | Package_builder.Failed _ -> true
        | Package_builder.Built _
        | Package_builder.Cached _
        | Package_builder.Skipped _ -> false)
    in
    let lane_result: Lane_result.t = {
      target = Build_lane.target lane;
      results = package_results;
      had_partial_failure;
    }
    in
    Some lane_result

let summarize = fun lanes package_states errors ->
  let lane_results =
    lanes
    |> List.filter_map ~fn:(lane_result_of_states package_states)
  in
  let lane_had_error = fun lane ->
    List.any errors ~fn:(fun error ->
      Riot_model.Target.equal (Build_lane.target lane) (Build_lane.target error.lane))
  in
  let completions =
    lanes
    |> List.map ~fn:(fun lane ->
      let lane_result =
        List.find lane_results ~fn:(fun result ->
          Riot_model.Target.equal (Lane_result.target result) (Build_lane.target lane))
      in
      {
        target = Build_lane.target lane;
        result_count =
          (match lane_result with
          | Some result -> List.length (Lane_result.results result)
          | None -> 0);
        had_partial_failure =
          lane_had_error lane
          || match lane_result with
          | Some result -> Lane_result.had_partial_failure result
          | None -> false;
      })
  in
  let had_failure =
    errors != []
    || List.any lane_results ~fn:Lane_result.had_partial_failure
  in
  {
    completions;
    lane_results;
    errors;
    had_failure;
  }

let run = fun ~parallelism ?(on_event = fun (_: event) -> ()) lanes ->
  let state = make_state lanes in
  let rec loop errors =
    let before = pending_counts lanes state.package_states in
    if before.awaiting_plan = 0 && before.awaiting_execution = 0 then
      summarize lanes state.package_states errors
    else if errors != [] then
      summarize lanes state.package_states errors
    else
      let planning_results =
        run_planning_round
          ~parallelism
          ~package_states:state.package_states
          ~on_event
          lanes
      in
      let planning_errors = collect_errors planning_results in
      let errors = errors @ planning_errors in
      if errors != [] then
        summarize lanes state.package_states errors
      else
        let execution_results =
          run_execution_round
            ~parallelism
            ~package_states:state.package_states
            ~on_event
            lanes
        in
        let execution_errors = collect_errors execution_results in
        let errors = errors @ execution_errors in
        if errors != [] then
          summarize lanes state.package_states errors
        else
          let after = pending_counts lanes state.package_states in
          if pending_counts_changed before after then
            loop errors
          else
            summarize lanes state.package_states (stalled_errors lanes state.package_states)
  in
  loop []
