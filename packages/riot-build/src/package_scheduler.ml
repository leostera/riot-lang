open Std
open Std.Collections
open Riot_planner

type work_item =
  | PlanPackage of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
    }
  | ExecuteAction of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      action: Action_node.t;
    }
  | FinalizePackage of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
    }

type execution_state = {
  prepared_execution: Package_builder.prepared_execution;
  completed_actions: (Graph.SimpleGraph.Node_id.t, Action_executor.execution_result) HashMap.t;
}

type finalized_source =
  | Planned
  | Executed

type package_state =
  | AwaitingPlan
  | AwaitingFinalization of execution_state
  | Finalized of {
      source: finalized_source;
      detailed_result: Package_builder.detailed_result;
    }

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
  | Remember_action_result of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      action_id: Graph.SimpleGraph.Node_id.t;
      result: Action_executor.execution_result;
    }

type planning_output =
  | PlanningRequiresExecution of {
      lane: Build_lane.locked Build_lane.t;
      execution_plan: Package_builder.execution_plan;
    }
  | PlanningFinalized of {
      lane: Build_lane.locked Build_lane.t;
      detailed_result: Package_builder.detailed_result;
    }

type action_output = {
  lane: Build_lane.locked Build_lane.t;
  package_key: Riot_model.Package.key;
  action: Action_node.t;
  result: Action_executor.execution_result;
}

type finalization_output =
  | FinalizedFromPlan of {
      lane: Build_lane.locked Build_lane.t;
      detailed_result: Package_builder.detailed_result;
    }
  | FinalizedFromExecution of {
      lane: Build_lane.locked Build_lane.t;
      detailed_result: Package_builder.detailed_result;
    }

type output =
  | Planned of planning_output
  | Executed_action of action_output
  | Finalized_package of finalization_output

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
  | PlanningStarted of { lane_count: int; package_count: int }
  | PlanningFinished of {
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
  | PackageActionGraphPlanned of {
      package: Riot_model.Package.t;
      build_target: Riot_model.Target.t;
      action_count: int;
      planned_at: Time.Instant.t;
    }
  | ExecutionStarted of { lane_count: int; package_count: int }
  | ExecutionFinished of {
      lane_count: int;
      package_count: int;
      finalized_count: int;
      built_count: int;
      failed_count: int;
      error_count: int;
    }

type graph_state = {
  package_states: (string, package_state) HashMap.t;
}

type pending_counts = { awaiting_plan: int; awaiting_finalization: int; finalized: int }

type planning_counts = {
  execution_required_count: int;
  finalized_count: int;
  cached_count: int;
  skipped_count: int;
  failed_count: int;
  error_count: int;
}

type execution_counts = {
  finalized_count: int;
  built_count: int;
  failed_count: int;
  error_count: int;
}

let lane_id = fun lane -> Riot_model.Target.to_string (Build_lane.target lane)

let package_key_id = fun package_key -> Riot_model.Package.key_to_string package_key

let package_state_key = fun lane package_key -> lane_id lane ^ "#" ^ package_key_id package_key

let plan_node_key = fun lane package_key -> package_state_key lane package_key ^ "#plan"

let finalize_node_key = fun lane package_key -> package_state_key lane package_key ^ "#finalize"

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
    |> List.map
      ~fn:(fun (package: Riot_model.Package.t) -> Riot_model.Package_name.to_string package.name)
  in
  "needs " ^ String.concat ", " failed_names

let dependency_failed_state = fun __tmp1 ->
  match __tmp1 with
  | Finalized {
      detailed_result = {
        result = {
          status = Package_builder.Failed _
          | Package_builder.Skipped _;
          package;
          _;
        };
        _;
      };
      _;
    } ->
      Some package
  | Finalized {
      detailed_result = {
        result = {
          status = Package_builder.Built _
          | Package_builder.Cached _;
          _;
        };
        _;
      };
      _;
    }
  | AwaitingPlan
  | AwaitingFinalization _ -> None

let remember_package_state = fun package_states lane package_key state ->
  let _ = HashMap.insert
    package_states
    ~key:(package_state_key lane package_key)
    ~value:state
  in
  ()

let get_package_state = fun package_states lane package_key ->
  HashMap.get
    package_states
    ~key:(package_state_key lane package_key)

let remember_action_result = fun package_states lane package_key action_id result ->
  match get_package_state package_states lane package_key with
  | Some (AwaitingFinalization execution_state) ->
      let _ = HashMap.insert execution_state.completed_actions ~key:action_id ~value:result in
      ()
  | Some AwaitingPlan ->
      panic
        ("package scheduler: action result recorded before planning for "
        ^ package_key_id package_key)
  | Some (Finalized _) ->
      panic
        ("package scheduler: action result recorded after finalization for "
        ^ package_key_id package_key)
  | None ->
      panic
        ("package scheduler: missing package state for action result " ^ package_key_id package_key)

let record_graph_update = fun handle lane ~package_key ~package graph_update ->
  Graph_scheduler.Handle.record
    handle
    (
      Apply_graph_update {
        lane;
        package_key;
        package;
        graph_update;
      }
    )

let record_package_state = fun handle lane package_key state ->
  Graph_scheduler.Handle.record
    handle
    (Set_package_state { lane; package_key; state })

let record_action_result = fun handle lane package_key (action: Action_node.t) result ->
  Graph_scheduler.Handle.record
    handle
    (
      Remember_action_result {
        lane;
        package_key;
        action_id = action.id;
        result;
      }
    )

let finalized_state = fun ~source detailed_result -> Finalized { source; detailed_result }

let finalize_result = fun graph ~source lane (detailed_result: Package_builder.detailed_result) ->
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
    (finalized_state ~source detailed_result)

let skipped_result_if_failed_dependencies = fun
  package_states
  lane
  package_key
  (package_node: Riot_planner.Package_graph.package_node Graph.SimpleGraph.node) ->
  let failed_dependencies =
    dependency_keys lane package_key
    |> List.filter_map
      ~fn:(fun dependency_key ->
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
      result =
        {
          package_key;
          package;
          status = Skipped { reason };
          ocamlc_warnings = [];
          duration = Time.Duration.zero;
        };
      graph_update = Some (Skipped_package { reason });
    }

let package_node = fun lane package_key ->
  Riot_planner.Package_graph.get_node_by_key
    (Build_lane.package_graph lane)
    package_key

let add_action_work = fun graph ~finalize_node_id lane package_key action_graph ->
  let action_node_ids: (Graph.SimpleGraph.Node_id.t, Graph_scheduler.Node_id.t) HashMap.t =
    HashMap.create ()
  in
  Action_graph.nodes action_graph
  |> List.for_each
    ~fn:(fun (action: Action_node.t) ->
      let node_id =
        Graph_scheduler.Handle.add_node graph ~payload:(ExecuteAction { lane; package_key; action })
      in
      let _ = HashMap.insert action_node_ids ~key:action.id ~value:node_id in
      ());
  Action_graph.nodes action_graph
  |> List.for_each
    ~fn:(fun (action: Action_node.t) ->
      let action_node_id =
        HashMap.get action_node_ids ~key:action.id
        |> Option.expect
          ~msg:("missing scheduler node for action " ^ Graph.SimpleGraph.Node_id.to_string action.id)
      in
      List.for_each
        action.deps
        ~fn:(fun dependency_id ->
          let dependency_node_id =
            HashMap.get action_node_ids ~key:dependency_id
            |> Option.expect
              ~msg:("missing scheduler dependency node for action "
              ^ Graph.SimpleGraph.Node_id.to_string dependency_id)
          in
          Graph_scheduler.Handle.add_dependency
            graph
            ~node:action_node_id
            ~depends_on:dependency_node_id);
      Graph_scheduler.Handle.add_dependency graph ~node:finalize_node_id ~depends_on:action_node_id)

let plan_package_work = fun ~package_states ~node_ids ~graph lane package_key ->
  match package_node lane package_key with
  | None ->
      Error { lane; reason = "package graph missing node for " ^ package_key_id package_key }
  | Some package_node -> (
      match skipped_result_if_failed_dependencies package_states lane package_key package_node with
      | Some detailed_result ->
          finalize_result graph ~source:Planned lane detailed_result;
          Ok (Planned (PlanningFinalized { lane; detailed_result }))
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
              finalize_result graph ~source:Planned lane detailed_result;
              Ok (Planned (PlanningFinalized { lane; detailed_result }))
          | Package_builder.Execution_required execution_plan ->
              let action_count = List.length (Action_graph.nodes execution_plan.action_graph) in
              if execution_plan.emit_visible_progress && action_count > 0 then
                Graph_scheduler.Handle.emit_event
                  graph
                  (
                    PackageActionGraphPlanned {
                      package;
                      build_target = Build_lane.target lane;
                      action_count;
                      planned_at = Time.Instant.now ();
                    }
                  );
              record_graph_update
                graph
                lane
                ~package_key
                ~package
                (Some (Package_builder.planned_graph_update execution_plan));
              (
                match Package_builder.prepare_execution
                  ~workspace:(Build_lane.workspace lane)
                  ~toolchain:(Build_lane.toolchain lane)
                  ~store:(Build_lane.store lane)
                  ~execution_plan
                  ~build_ctx:(Build_lane.build_ctx lane) with
                | Error detailed_result ->
                    finalize_result graph ~source:Planned lane detailed_result;
                    Ok (Planned (PlanningFinalized { lane; detailed_result }))
                | Ok prepared_execution ->
                    let finalize_node_id =
                      HashMap.get node_ids ~key:(finalize_node_key lane package_key)
                      |> Option.expect
                        ~msg:("missing finalize node for " ^ package_state_key lane package_key)
                    in
                    record_package_state
                      graph
                      lane
                      package_key
                      (AwaitingFinalization {
                        prepared_execution;
                        completed_actions = HashMap.create ();
                      });
                    add_action_work
                      graph
                      ~finalize_node_id
                      lane
                      package_key
                      execution_plan.action_graph;
                    Ok (Planned (PlanningRequiresExecution { lane; execution_plan }))
              )
    )

let execute_action_work = fun ~package_states ~graph lane package_key action ->
  match get_package_state package_states lane package_key with
  | Some (AwaitingFinalization execution_state) ->
      let result =
        Package_builder.execute_action
          ~store:(Build_lane.store lane)
          ~prepared_execution:execution_state.prepared_execution
          ~build_ctx:(Build_lane.build_ctx lane)
          ~completed:execution_state.completed_actions
          action
      in
      record_action_result graph lane package_key action result;
      Ok (
        Executed_action {
          lane;
          package_key;
          action;
          result;
        }
      )
  | Some AwaitingPlan ->
      Error {
        lane;
        reason = "action execution ran before planning for " ^ package_key_id package_key;
      }
  | Some (Finalized _) ->
      Error {
        lane;
        reason = "action execution reran after finalization for " ^ package_key_id package_key;
      }
  | None ->
      Error {
        lane;
        reason = "package state missing for action execution " ^ package_key_id package_key;
      }

let finalize_package_work = fun ~package_states ~graph lane package_key ->
  match get_package_state package_states lane package_key with
  | Some (Finalized { source = Planned; detailed_result }) ->
      Ok (Finalized_package (FinalizedFromPlan { lane; detailed_result }))
  | Some (Finalized { source = Executed; detailed_result }) ->
      Ok (Finalized_package (FinalizedFromExecution { lane; detailed_result }))
  | Some (AwaitingFinalization execution_state) ->
      let detailed_result =
        Package_builder.finalize_execution
          ~workspace:(Build_lane.workspace lane)
          ~store:(Build_lane.store lane)
          ~prepared_execution:execution_state.prepared_execution
          ~completed:execution_state.completed_actions
          ~build_ctx:(Build_lane.build_ctx lane)
      in
      finalize_result graph ~source:Executed lane detailed_result;
      Ok (Finalized_package (FinalizedFromExecution { lane; detailed_result }))
  | Some AwaitingPlan ->
      Error {
        lane;
        reason = "package finalization ran before planning for " ^ package_key_id package_key;
      }
  | None ->
      Error {
        lane;
        reason = "package state missing for finalization " ^ package_key_id package_key;
      }

let make_state = fun lanes ->
  let package_states: (string, package_state) HashMap.t = HashMap.create () in
  List.for_each
    lanes
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.for_each
        ~fn:(fun package_key ->
          remember_package_state package_states lane package_key AwaitingPlan));
  { package_states }

let make_graph = fun state lanes ->
  let graph =
    Graph_scheduler.Graph.create
      ~apply_mutation:(fun _ mutation ->
        match mutation with
        | Apply_graph_update {
            lane;
            package_key;
            package;
            graph_update;
          } ->
            Package_builder.apply_graph_update
              (Build_lane.package_graph lane)
              package_key
              package
              graph_update
        | Set_package_state { lane; package_key; state = next_state } ->
            remember_package_state state.package_states lane package_key next_state
        | Remember_action_result {
            lane;
            package_key;
            action_id;
            result;
          } ->
            remember_action_result state.package_states lane package_key action_id result)
      ()
  in
  let node_ids: (string, Graph_scheduler.Node_id.t) HashMap.t = HashMap.create () in
  List.for_each
    lanes
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.for_each
        ~fn:(fun package_key ->
          let plan_node_id =
            Graph_scheduler.Graph.add_node graph ~payload:(PlanPackage { lane; package_key })
          in
          let finalize_node_id =
            Graph_scheduler.Graph.add_node graph ~payload:(FinalizePackage { lane; package_key })
          in
          let _ =
            HashMap.insert
              node_ids
              ~key:(plan_node_key lane package_key)
              ~value:plan_node_id
          in
          let _ =
            HashMap.insert
              node_ids
              ~key:(finalize_node_key lane package_key)
              ~value:finalize_node_id
          in
          ()));
  List.for_each
    lanes
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.for_each
        ~fn:(fun package_key ->
          let plan_node_id =
            HashMap.get node_ids ~key:(plan_node_key lane package_key)
            |> Option.expect ~msg:("missing plan node for " ^ package_state_key lane package_key)
          in
          let finalize_node_id =
            HashMap.get node_ids ~key:(finalize_node_key lane package_key)
            |> Option.expect
              ~msg:("missing finalize node for " ^ package_state_key lane package_key)
          in
          Graph_scheduler.Graph.add_dependency graph ~node:finalize_node_id ~depends_on:plan_node_id;
          dependency_keys lane package_key
          |> List.for_each
            ~fn:(fun dependency_key ->
              match HashMap.get node_ids ~key:(finalize_node_key lane dependency_key) with
              | Some dependency_finalize_node_id ->
                  Graph_scheduler.Graph.add_dependency
                    graph
                    ~node:plan_node_id
                    ~depends_on:dependency_finalize_node_id
              | None -> ())));
  (graph, node_ids)

let total_package_count = fun lanes ->
  lanes
  |> List.fold_left
    ~init:0
    ~fn:(fun count lane -> count + List.length (Build_lane.package_keys lane))

let deferred_package_count = fun lanes ->
  lanes
  |> List.fold_left
    ~init:0
    ~fn:(fun count lane ->
      count
      + (
        Build_lane.package_keys lane
        |> List.filter ~fn:(fun package_key -> dependency_keys lane package_key != [])
        |> List.length
      ))

let pending_counts = fun lanes package_states ->
  lanes
  |> List.fold_left
    ~init:{ awaiting_plan = 0; awaiting_finalization = 0; finalized = 0 }
    ~fn:(fun counts lane ->
      Build_lane.package_keys lane
      |> List.fold_left
        ~init:counts
        ~fn:(fun counts package_key ->
          match get_package_state package_states lane package_key with
          | Some AwaitingPlan -> { counts with awaiting_plan = counts.awaiting_plan + 1 }
          | Some (AwaitingFinalization _) ->
              { counts with awaiting_finalization = counts.awaiting_finalization + 1 }
          | Some (Finalized _) -> { counts with finalized = counts.finalized + 1 }
          | None -> counts))

let pending_descriptions = fun lanes package_states ->
  lanes
  |> List.flat_map
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.filter_map
        ~fn:(fun package_key ->
          match get_package_state package_states lane package_key with
          | Some AwaitingPlan -> Some ("awaiting plan: " ^ package_key_id package_key)
          | Some (AwaitingFinalization _) ->
              Some ("awaiting finalization: " ^ package_key_id package_key)
          | Some (Finalized _)
          | None -> None))

let stalled_errors = fun lanes package_states ->
  let pending = pending_descriptions lanes package_states in
  if pending = [] then
    []
  else
    let reason =
      "package scheduler made no progress with pending work: " ^ String.concat ", " pending
    in
    List.map lanes ~fn:(fun lane -> { lane; reason })

let cleanup_pending_execution = fun package_states ->
  HashMap.to_list package_states
  |> List.for_each
    ~fn:(fun (_, state) ->
      match state with
      | AwaitingFinalization execution_state ->
          Sandbox.cleanup execution_state.prepared_execution.sandbox
      | AwaitingPlan
      | Finalized _ -> ())

let summarize_planning_results = fun results ->
  List.fold_left
    results
    ~init:{
      execution_required_count = 0;
      finalized_count = 0;
      cached_count = 0;
      skipped_count = 0;
      failed_count = 0;
      error_count = 0;
    }
    ~fn:(fun counts (result: (work_item, output, error) Graph_scheduler.node_result) ->
      match result.payload with
      | PlanPackage _ -> (
          match result.outcome with
          | Error _ -> { counts with error_count = counts.error_count + 1 }
          | Ok (Planned (PlanningRequiresExecution _)) ->
              { counts with execution_required_count = counts.execution_required_count + 1 }
          | Ok (Planned (PlanningFinalized { detailed_result; _ })) ->
              let counts = { counts with finalized_count = counts.finalized_count + 1 } in
              (
                match detailed_result.result.status with
                | Package_builder.Cached _ ->
                    { counts with cached_count = counts.cached_count + 1 }
                | Package_builder.Skipped _ ->
                    { counts with skipped_count = counts.skipped_count + 1 }
                | Package_builder.Failed _ ->
                    { counts with failed_count = counts.failed_count + 1 }
                | Package_builder.Built _ -> counts
              )
          | Ok (Executed_action _)
          | Ok (Finalized_package _) -> counts
        )
      | ExecuteAction _
      | FinalizePackage _ -> counts)

let summarize_execution_results = fun results ->
  List.fold_left
    results
    ~init:{
      finalized_count = 0;
      built_count = 0;
      failed_count = 0;
      error_count = 0;
    }
    ~fn:(fun counts (result: (work_item, output, error) Graph_scheduler.node_result) ->
      match result.payload with
      | ExecuteAction _
      | FinalizePackage _ -> (
          match result.outcome with
          | Error _ -> { counts with error_count = counts.error_count + 1 }
          | Ok (Finalized_package (FinalizedFromExecution { detailed_result; _ })) ->
              let counts = { counts with finalized_count = counts.finalized_count + 1 } in
              (
                match detailed_result.result.status with
                | Package_builder.Built _ -> { counts with built_count = counts.built_count + 1 }
                | Package_builder.Failed _ ->
                    { counts with failed_count = counts.failed_count + 1 }
                | Package_builder.Cached _
                | Package_builder.Skipped _ -> counts
              )
          | Ok (Finalized_package (FinalizedFromPlan _))
          | Ok (Executed_action _)
          | Ok (Planned _) -> counts
        )
      | PlanPackage _ -> counts)

let collect_errors = fun results ->
  List.filter_map
    results
    ~fn:(fun (result: (_, _, error) Graph_scheduler.node_result) ->
      match result.outcome with
      | Error err -> Some err
      | Ok _ -> None)

let lane_result_of_states = fun package_states lane ->
  let package_results =
    Build_lane.package_keys lane
    |> List.filter_map
      ~fn:(fun package_key ->
        match get_package_state package_states lane package_key with
        | Some (Finalized { detailed_result; _ }) -> Some detailed_result.result
        | Some AwaitingPlan
        | Some (AwaitingFinalization _)
        | None -> None)
  in
  if package_results = [] then
    None
  else
    let had_partial_failure =
      List.any
        package_results
        ~fn:(fun (result: Package_builder.build_result) ->
          match result.status with
          | Package_builder.Failed _ -> true
          | Package_builder.Built _
          | Package_builder.Cached _
          | Package_builder.Skipped _ -> false)
    in
    Some ({ target = Build_lane.target lane; results = package_results; had_partial_failure }:
      Lane_result.t)

let summarize = fun lanes package_states errors ->
  let lane_results =
    lanes
    |> List.filter_map ~fn:(lane_result_of_states package_states)
  in
  let lane_had_error lane =
    List.any
      errors
      ~fn:(fun error ->
        Riot_model.Target.equal
          (Build_lane.target lane)
          (Build_lane.target error.lane))
  in
  let completions =
    lanes
    |> List.map
      ~fn:(fun lane ->
        let lane_result =
          List.find
            lane_results
            ~fn:(fun result ->
              Riot_model.Target.equal
                (Lane_result.target result)
                (Build_lane.target lane))
        in
        {
          target = Build_lane.target lane;
          result_count =
            (
              match lane_result with
              | Some result -> List.length (Lane_result.results result)
              | None -> 0
            );
          had_partial_failure =
            lane_had_error lane || match lane_result with
            | Some result -> Lane_result.had_partial_failure result
            | None ->
                false;
        })
  in
  let had_failure = errors != [] || List.any lane_results ~fn:Lane_result.had_partial_failure in
  {
    completions;
    lane_results;
    errors;
    had_failure;
  }

let run = fun ~parallelism ?(on_event = fun (_:event) -> ()) lanes ->
  let state = make_state lanes in
  let lane_count = List.length lanes in
  let package_count = total_package_count lanes in
  if package_count = 0 then
    summarize lanes state.package_states []
  else
    let (graph, node_ids) = make_graph state lanes in
    on_event (PlanningStarted { lane_count; package_count });
  try
    let results =
      Graph_scheduler.run
        ~config:(Graph_scheduler.Run_config.make
          ~parallelism
          ~mode:Graph_scheduler.Run_config.Continue_on_failure
          ())
        ~on_event
        ~graph
        ~execute:(fun ~graph ~node:_ ~payload ->
          match payload with
          | PlanPackage { lane; package_key } ->
              plan_package_work
                ~package_states:state.package_states
                ~node_ids
                ~graph
                lane
                package_key
          | ExecuteAction { lane; package_key; action } ->
              execute_action_work
                ~package_states:state.package_states
                ~graph
                lane
                package_key
                action
          | FinalizePackage { lane; package_key } ->
              finalize_package_work ~package_states:state.package_states ~graph lane package_key)
      |> fun run_result -> run_result.results
    in
    let planning_counts = summarize_planning_results results in
    on_event
      (
        PlanningFinished {
          lane_count;
          package_count;
          deferred_count = deferred_package_count lanes;
          execution_required_count = planning_counts.execution_required_count;
          finalized_count = planning_counts.finalized_count;
          cached_count = planning_counts.cached_count;
          skipped_count = planning_counts.skipped_count;
          failed_count = planning_counts.failed_count;
          error_count = planning_counts.error_count;
        }
      );
    if planning_counts.execution_required_count > 0 then (
      let execution_counts = summarize_execution_results results in
      on_event
        (ExecutionStarted { lane_count; package_count = planning_counts.execution_required_count });
      on_event
        (
          ExecutionFinished {
            lane_count;
            package_count = planning_counts.execution_required_count;
            finalized_count = execution_counts.finalized_count;
            built_count = execution_counts.built_count;
            failed_count = execution_counts.failed_count;
            error_count = execution_counts.error_count;
          }
        )
    );
    cleanup_pending_execution state.package_states;
    let errors = collect_errors results in
    let pending = pending_counts lanes state.package_states in
    let errors =
      if errors = [] && (pending.awaiting_plan > 0 || pending.awaiting_finalization > 0) then
        stalled_errors lanes state.package_states
      else
        errors
    in
    summarize lanes state.package_states errors
  with
  | exn ->
      cleanup_pending_execution state.package_states;
      raise exn
