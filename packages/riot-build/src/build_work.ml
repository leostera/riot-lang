open Std
open Std.Collections
open Std.Result.Syntax

type plan_package = {
  lane: Build_lane.locked Build_lane.t;
  package_key: Riot_model.Package.key;
}

type t =
  | BuildPackage of plan_package

type mutation =
  | Apply_graph_update of {
      lane: Build_lane.locked Build_lane.t;
      package_key: Riot_model.Package.key;
      package: Riot_model.Package.t;
      graph_update: Package_builder.graph_update option;
    }

type output =
  | PackageCompleted of {
      lane: Build_lane.locked Build_lane.t;
      detailed_result: Package_builder.detailed_result;
    }

type error = {
  lane: Build_lane.locked Build_lane.t;
  reason: string;
}

type run_result = {
  lanes: Build_lane.locked Build_lane.t list;
  task_results: (t * (output, error) result) list;
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

let target = function
  | BuildPackage { lane; _ } -> Build_lane.target lane

let plan_package = fun lane package_key -> { lane; package_key }

let initial_plan_packages = fun lane ->
  Build_lane.package_keys lane
  |> List.map ~fn:(plan_package lane)

let plan_package_key = fun (plan_package: plan_package) -> plan_package.package_key

let plan_package_target = fun (plan_package: plan_package) -> Build_lane.target plan_package.lane

let lane_id = fun lane -> Riot_model.Target.to_string (Build_lane.target lane)

let package_key_id = fun package_key -> Riot_model.Package.key_to_string package_key

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

let prepare_lane = fun context spec ~toolchain target ->
  Build_lane.prepare context spec ~target ~toolchain

let prepare_lanes = fun context spec ~toolchain ->
  let targets =
    Riot_model.Target.Set.to_list (Resolved_build.targets spec)
    |> List.sort ~compare:Riot_model.Target.compare
  in
  let release_lanes = fun lanes -> List.for_each lanes ~fn:Build_lane.release in
  let rec loop prepared = function
    | [] -> Ok (List.reverse prepared)
    | target :: rest -> (
        match prepare_lane context spec ~toolchain target with
        | Ok lane -> loop (lane :: prepared) rest
        | Error _ as error ->
            release_lanes prepared;
            error
      )
  in
  loop [] targets

let record_graph_update = fun handle lane (detailed_result: Package_builder.detailed_result) ->
  Graph_scheduler.Handle.record
    handle
    (Apply_graph_update {
      lane;
      package_key = detailed_result.result.package_key;
      package = detailed_result.result.package;
      graph_update = detailed_result.graph_update;
    })

let execute = fun ~graph ~node:_ ~payload ->
  match payload with
  | BuildPackage ({ lane; package_key } as work) -> (
      match Riot_planner.Package_graph.get_node_by_key (Build_lane.package_graph lane) package_key with
      | None ->
          Error {
            lane;
            reason = "package graph missing node for " ^ package_key_id work.package_key;
          }
      | Some node ->
          let failed_dependencies =
            dependency_nodes lane package_key
            |> List.filter ~fn:is_failed_node
          in
          let detailed_result =
            if failed_dependencies != [] then
              let package = Riot_planner.Package_graph.get_package node.value in
              let reason = skipped_reason failed_dependencies in
              Package_builder.{
                result = {
                  package_key;
                  package;
                  status = Skipped { reason };
                  ocamlc_warnings = [];
                  duration = Time.Duration.zero;
                };
                graph_update = Some (Skipped_package { reason });
              }
            else
              let package = Riot_planner.Package_graph.get_package node.value in
              Package_builder.build_detailed
                ~workspace:(Build_lane.workspace lane)
                ~toolchain:(Build_lane.toolchain lane)
                ~store:(Build_lane.store lane)
                ~package_graph:(Build_lane.package_graph lane)
                ~package_key
                ~package
                ~build_ctx:(Build_lane.build_ctx lane)
          in
          record_graph_update graph lane detailed_result;
          Ok (PackageCompleted {
            lane;
            detailed_result;
          })
    )

let release_lanes = fun lanes ->
  List.for_each lanes ~fn:Build_lane.release

let lane_package_graph_key = fun lane package_key ->
  lane_id lane ^ "#" ^ package_key_id package_key

let make_graph = fun lanes ->
  let graph =
    Graph_scheduler.Graph.create
      ~apply_mutation:(fun _ mutation ->
        match mutation with
        | Apply_graph_update { lane; package_key; package; graph_update } ->
            apply_graph_update lane package_key package graph_update)
      ()
  in
  let node_ids: (string, Graph_scheduler.Node_id.t) HashMap.t = HashMap.create () in
  List.for_each lanes
    ~fn:(fun lane ->
      initial_plan_packages lane
      |> List.for_each ~fn:(fun plan ->
        let node_id =
          Graph_scheduler.Graph.add_node graph ~payload:(BuildPackage plan)
        in
        let _ = HashMap.insert
          node_ids
          ~key:(lane_package_graph_key lane plan.package_key)
          ~value:node_id
        in
        ()));
  List.for_each lanes
    ~fn:(fun lane ->
      initial_plan_packages lane
      |> List.for_each ~fn:(fun plan ->
        let node_key = lane_package_graph_key lane plan.package_key in
        let node_id =
          HashMap.get node_ids ~key:node_key
          |> Option.expect ~msg:("missing graph node for " ^ node_key)
        in
        dependency_keys lane plan.package_key
        |> List.for_each ~fn:(fun dependency_key ->
          let dependency_node_key = lane_package_graph_key lane dependency_key in
          let dependency_node_id =
            HashMap.get node_ids ~key:dependency_node_key
            |> Option.expect ~msg:("missing graph node for " ^ dependency_node_key)
          in
          Graph_scheduler.Graph.add_dependency
            graph
            ~node:node_id
            ~depends_on:dependency_node_id)));
  graph

let run = fun context lanes ->
  let task_results =
    try
      let graph = make_graph lanes in
      Graph_scheduler.run
        ~config:(Graph_scheduler.Run_config.make
          ~parallelism:context.Build_context.parallelism
          ~mode:Graph_scheduler.Run_config.Continue_on_failure
          ())
        ~on_event:(fun () -> ())
        ~graph
        ~execute
      |> fun results ->
        List.map results.results
          ~fn:(fun (result: (t, output, error) Graph_scheduler.node_result) ->
            result.payload, result.outcome)
    with
    | exn ->
        release_lanes lanes;
        raise exn
  in
  release_lanes lanes;
  {
    lanes;
    task_results;
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

let summarize = fun run_result ->
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
    run_result.task_results
    |> List.filter_map ~fn:(fun (_, outcome) ->
      match outcome with
      | Error err -> Some err
      | Ok (PackageCompleted { lane; detailed_result }) ->
          remember_result lane detailed_result.result;
          None)
  in
  let lane_results =
    run_result.lanes
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
    run_result.lanes
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
