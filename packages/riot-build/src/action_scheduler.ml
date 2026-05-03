open Std
open Std.Collections
open Riot_planner

type action_error = Action_executor.action_error =
  | ExecutionFailed of { message: string }
  | OutputsNotCreated of {
      missing: Path.t list;
    }
  | DependenciesFailed of {
      failed: Graph.SimpleGraph.Node_id.t list;
    }

type action_status = Action_executor.action_status =
  | Cached of Riot_store.Artifact.t
  | Executed of Riot_store.Artifact.t
  | Failed of action_error
  | Skipped

type execution_result = Action_executor.execution_result = {
  node_id: Graph.SimpleGraph.Node_id.t;
  status: action_status;
  ocamlc_warnings: string list;
  duration: Time.Duration.t;
  started_at: Time.Instant.t;
  completed_at: Time.Instant.t;
}

type completed_action = {
  node: Action_node.t;
  result: execution_result;
}

type mutation =
  | Remember_result of completed_action

type t = {
  completed_actions: completed_action list;
  first_failure: action_error option;
  ocamlc_warnings: string list;
}

let remember_result = fun completed_results (completed_action: completed_action) ->
  let _ =
    HashMap.insert completed_results ~key:completed_action.node.id ~value:completed_action.result
  in
  ()

let make_graph = fun completed_results action_graph ->
  let graph =
    Graph_scheduler.Graph.create
      ~apply_mutation:(fun _ mutation ->
        match mutation with
        | Remember_result completed_action -> remember_result completed_results completed_action)
      ()
  in
  let node_ids: (Graph.SimpleGraph.Node_id.t, Graph_scheduler.Node_id.t) HashMap.t =
    HashMap.create ()
  in
  Action_graph.nodes action_graph
  |> List.for_each
    ~fn:(fun (node: Action_node.t) ->
      let node_id = Graph_scheduler.Graph.add_node graph ~payload:node in
      let _ = HashMap.insert node_ids ~key:node.id ~value:node_id in
      ());
  Action_graph.nodes action_graph
  |> List.for_each
    ~fn:(fun (node: Action_node.t) ->
      let node_id =
        HashMap.get node_ids ~key:node.id
        |> Option.expect
          ~msg:("missing scheduler node for action " ^ Graph.SimpleGraph.Node_id.to_string node.id)
      in
      List.for_each
        node.deps
        ~fn:(fun dependency_id ->
          let dependency_node_id =
            HashMap.get node_ids ~key:dependency_id
            |> Option.expect
              ~msg:("missing scheduler dependency node for action "
              ^ Graph.SimpleGraph.Node_id.to_string dependency_id)
          in
          Graph_scheduler.Graph.add_dependency graph ~node:node_id ~depends_on:dependency_node_id));
  graph

let rec find_first_map = fun items ~fn ->
  match items with
  | [] -> None
  | item :: rest -> (
      match fn item with
      | Some _ as result -> result
      | None -> find_first_map rest ~fn
    )

let first_failure_of_completed_actions = fun completed_actions ->
  find_first_map
    completed_actions
    ~fn:(fun completed_action ->
      match completed_action.result.status with
      | Failed err -> Some err
      | Cached _
      | Executed _
      | Skipped -> None)

let ocamlc_warnings_of_completed_actions = fun completed_actions ->
  let seen = HashSet.create () in
  completed_actions
  |> List.fold_left
    ~init:[]
    ~fn:(fun acc completed_action ->
      List.fold_left
        completed_action.result.ocamlc_warnings
        ~init:acc
        ~fn:(fun acc warning ->
          if HashSet.contains seen ~value:warning then
            acc
          else
            let _ = HashSet.insert seen ~value:warning in
            acc @ [ warning ]))

let find_result = fun (result: t) (node: Action_node.t) ->
  find_first_map
    result.completed_actions
    ~fn:(fun completed_action ->
      if Graph.SimpleGraph.Node_id.eq completed_action.node.id node.id then
        Some completed_action.result
      else
        None)

let summarize_completed = fun ~action_graph ~completed_results ->
  let completed_actions =
    Action_graph.nodes action_graph
    |> List.filter_map
      ~fn:(fun (node: Action_node.t) ->
        match HashMap.get completed_results ~key:node.id with
        | Some result -> Some { node; result }
        | None -> None)
  in
  {
    completed_actions;
    first_failure = first_failure_of_completed_actions completed_actions;
    ocamlc_warnings = ocamlc_warnings_of_completed_actions completed_actions;
  }

let run = fun ~action_graph ~sandbox ~store ~session_id toolchain ~concurrency ->
  let completed_results: (Graph.SimpleGraph.Node_id.t, execution_result) HashMap.t =
    HashMap.create ()
  in
  let sandbox_dir = Sandbox.get_dir sandbox in
  let graph = make_graph completed_results action_graph in
  let _ =
    Graph_scheduler.run
      ~config:(Graph_scheduler.Run_config.make
        ~parallelism:concurrency
        ~mode:Graph_scheduler.Run_config.Continue_on_failure
        ())
      ~on_event:(fun () -> ())
      ~graph
      ~execute:(fun ~graph ~node:_ ~payload ->
        let result =
          Action_executor.execute_node
            ~completed:completed_results
            ~store
            ~session_id
            toolchain
            sandbox_dir
            payload
        in
        Graph_scheduler.Handle.record graph (Remember_result { node = payload; result });
        Ok result)
  in
  summarize_completed ~action_graph ~completed_results
