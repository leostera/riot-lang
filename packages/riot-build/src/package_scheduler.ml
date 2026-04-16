open Std
open Std.Collections

type package_work = {
  lane: Build_lane.locked Build_lane.t;
  package_key: Riot_model.Package.key;
}

type work =
  | PlanPackage of package_work
  | ExecutePackage of package_work
  | FinalizePackage of package_work

type package_state =
  | AwaitingPlan
  | AwaitingExecution of Package_builder.execution_plan
  | ReadyToFinalize of Package_builder.detailed_result
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

type output =
  | PackagePlanned of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      needs_execution: bool;
    }
  | PackageExecuted of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      ran: bool;
    }
  | PackageCompleted of {
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

type prepared_graph = {
  graph: (work, mutation) Graph_scheduler.Graph.t;
  package_states: (string, package_state) HashMap.t;
}

type node_phase =
  | Plan
  | Execute
  | Finalize

let lane_id = fun lane -> Riot_model.Target.to_string (Build_lane.target lane)

let package_key_id = fun package_key -> Riot_model.Package.key_to_string package_key

let package_state_key = fun lane package_key ->
  lane_id lane ^ "#" ^ package_key_id package_key

let package_node_key = fun phase lane package_key ->
  let phase_id =
    match phase with
    | Plan -> "plan"
    | Execute -> "execute"
    | Finalize -> "finalize"
  in
  package_state_key lane package_key ^ "#" ^ phase_id

let dependency_nodes = fun lane package_key ->
  match Riot_planner.Package_graph.get_node_by_key (Build_lane.package_graph lane) package_key with
  | None -> []
  | Some node ->
      Riot_planner.Package_graph.get_dependencies_for_node (Build_lane.package_graph lane) node

let dependency_keys = fun lane package_key ->
  dependency_nodes lane package_key
  |> List.map ~fn:Riot_planner.Package_graph.get_key

let is_failed_node = function
  | Riot_planner.Package_graph.Failed _
  | Riot_planner.Package_graph.Skipped _ -> true
  | Riot_planner.Package_graph.Unplanned _
  | Riot_planner.Package_graph.Planned _
  | Riot_planner.Package_graph.Cached _
  | Riot_planner.Package_graph.Built _ -> false

let skipped_reason = fun failed_nodes ->
  let failed_names =
    failed_nodes
    |> List.map ~fn:Riot_planner.Package_graph.get_package
    |> List.map ~fn:(fun (package: Riot_model.Package.t) ->
      Riot_model.Package_name.to_string package.name)
  in
  "needs " ^ String.concat ", " failed_names

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

let skipped_result_if_failed_dependencies =
  fun
    lane
    package_key
    (package_node: Riot_planner.Package_graph.package_node Graph.SimpleGraph.node) ->
  let failed_dependencies =
    dependency_nodes lane package_key
    |> List.filter ~fn:is_failed_node
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
        let finalize = fun detailed_result ->
          record_package_state graph lane package_key (ReadyToFinalize detailed_result);
          Ok (PackagePlanned {
            lane;
            package_key;
            needs_execution = false;
          })
        in
        (match skipped_result_if_failed_dependencies lane package_key package_node with
        | Some detailed_result -> finalize detailed_result
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
                finalize detailed_result
            | Package_builder.Execution_required execution_plan ->
                record_graph_update
                  graph
                  lane
                  ~package_key
                  ~package
                  (Some (Package_builder.planned_graph_update execution_plan));
                record_package_state graph lane package_key (AwaitingExecution execution_plan);
                Ok (PackagePlanned {
                  lane;
                  package_key;
                  needs_execution = true;
                }))

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
        record_package_state graph lane package_key (ReadyToFinalize detailed_result);
        Ok (PackageExecuted {
          lane;
          package_key;
          ran = true;
        })
    | Some (ReadyToFinalize _)
    | Some (Finalized _) ->
        Ok (PackageExecuted {
          lane;
          package_key;
          ran = false;
        })
    | Some AwaitingPlan ->
        Error {
          lane;
          reason = "package execution ran before planning for " ^ package_key_id package_key;
        }
    | None ->
        Error {
          lane;
          reason = "package state missing for execution " ^ package_key_id package_key;
        }

let finalize_package_work =
  fun
    ~package_states
    ~graph
    ({ lane; package_key }: package_work) ->
    match get_package_state package_states lane package_key with
    | Some (ReadyToFinalize detailed_result) ->
        record_graph_update
          graph
          lane
          ~package_key:detailed_result.result.package_key
          ~package:detailed_result.result.package
          detailed_result.graph_update;
        record_package_state graph lane package_key (Finalized detailed_result);
        Ok (PackageCompleted {
          lane;
          detailed_result;
        })
    | Some (Finalized detailed_result) ->
        Ok (PackageCompleted {
          lane;
          detailed_result;
        })
    | Some (AwaitingExecution _) ->
        Error {
          lane;
          reason = "package finalization ran before execution for " ^ package_key_id package_key;
        }
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

let execute = fun ~package_states ~graph ~node:_ ~payload ->
  match payload with
  | PlanPackage work -> plan_package_work ~package_states ~graph work
  | ExecutePackage work -> execute_package_work ~package_states ~graph work
  | FinalizePackage work -> finalize_package_work ~package_states ~graph work

let make_graph = fun lanes ->
  let package_states: (string, package_state) HashMap.t = HashMap.create () in
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
  List.for_each lanes
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.for_each ~fn:(fun package_key ->
        let work = { lane; package_key } in
        remember_package_state package_states lane package_key AwaitingPlan;
        let phases = [
          (Plan, PlanPackage work);
          (Execute, ExecutePackage work);
          (Finalize, FinalizePackage work);
        ] in
        List.for_each phases ~fn:(fun (phase, payload) ->
          let node_id = Graph_scheduler.Graph.add_node graph ~payload in
          let _ = HashMap.insert
            node_ids
            ~key:(package_node_key phase lane package_key)
            ~value:node_id
          in
          ())));
  List.for_each lanes
    ~fn:(fun lane ->
      Build_lane.package_keys lane
      |> List.for_each ~fn:(fun package_key ->
        let plan_node_id =
          HashMap.get node_ids ~key:(package_node_key Plan lane package_key)
          |> Option.expect ~msg:("missing graph node for " ^ package_node_key Plan lane package_key)
        in
        let execute_node_id =
          HashMap.get node_ids ~key:(package_node_key Execute lane package_key)
          |> Option.expect ~msg:("missing graph node for " ^ package_node_key Execute lane package_key)
        in
        let finalize_node_id =
          HashMap.get node_ids ~key:(package_node_key Finalize lane package_key)
          |> Option.expect ~msg:("missing graph node for " ^ package_node_key Finalize lane package_key)
        in
        Graph_scheduler.Graph.add_dependency
          graph
          ~node:execute_node_id
          ~depends_on:plan_node_id;
        Graph_scheduler.Graph.add_dependency
          graph
          ~node:finalize_node_id
          ~depends_on:execute_node_id;
        dependency_keys lane package_key
        |> List.for_each ~fn:(fun dependency_key ->
          let dependency_finalize_node_id =
            HashMap.get node_ids ~key:(package_node_key Finalize lane dependency_key)
            |> Option.expect
              ~msg:("missing graph node for " ^ package_node_key Finalize lane dependency_key)
          in
          Graph_scheduler.Graph.add_dependency
            graph
            ~node:plan_node_id
            ~depends_on:dependency_finalize_node_id)));
  {
    graph;
    package_states;
  }

let lane_result_of_results = fun lane package_results ->
  let lane_had_partial_failure =
    List.any package_results ~fn:(fun (result: Package_builder.build_result) ->
      match result.status with
      | Package_builder.Failed _ -> true
      | Package_builder.Built _
      | Package_builder.Cached _
      | Package_builder.Skipped _ -> false)
  in
  Lane_result.{
    target = Build_lane.target lane;
    results = package_results;
    had_partial_failure = lane_had_partial_failure;
  }

let summarize = fun lanes task_results ->
  let lane_results_by_id: (string, (string, Package_builder.build_result) HashMap.t) HashMap.t = HashMap.create () in
  let remember_result = fun lane (result: Package_builder.build_result) ->
    let key = lane_id lane in
    let lane_results =
      match HashMap.get lane_results_by_id ~key with
      | Some lane_results -> lane_results
      | None ->
          let lane_results = HashMap.create () in
          let _ = HashMap.insert lane_results_by_id ~key ~value:lane_results in
          lane_results
    in
    let _ = HashMap.insert lane_results ~key:(package_key_id result.package_key) ~value:result in
    ()
  in
  let errors =
    task_results
    |> List.filter_map ~fn:(fun (_, outcome) ->
      match outcome with
      | Error err -> Some err
      | Ok (PackageCompleted { lane; detailed_result }) ->
          remember_result lane detailed_result.result;
          None
      | Ok (PackagePlanned _)
      | Ok (PackageExecuted _) -> None)
  in
  let lane_results =
    lanes
    |> List.filter_map ~fn:(fun lane ->
      let ordered_results =
        match HashMap.get lane_results_by_id ~key:(lane_id lane) with
        | None -> []
        | Some results_by_key ->
            Build_lane.package_keys lane
            |> List.filter_map ~fn:(fun package_key ->
              HashMap.get results_by_key ~key:(package_key_id package_key))
      in
      if ordered_results = [] then
        None
      else
        Some (lane_result_of_results lane ordered_results))
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

let run = fun ~parallelism lanes ->
  let prepared_graph = make_graph lanes in
  Graph_scheduler.run
    ~config:(Graph_scheduler.Run_config.make
      ~parallelism
      ~mode:Graph_scheduler.Run_config.Continue_on_failure
      ())
    ~on_event:(fun () -> ())
    ~graph:prepared_graph.graph
    ~execute:(execute ~package_states:prepared_graph.package_states)
  |> fun results ->
    let task_results =
      List.map results.results
        ~fn:(fun (result: (work, output, error) Graph_scheduler.node_result) ->
          result.payload, result.outcome)
    in
    summarize lanes task_results
