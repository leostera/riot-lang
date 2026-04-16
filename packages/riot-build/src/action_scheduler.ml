open Std
open Std.Collections
open Riot_planner

type action_error = Action_executor.action_error =
  | ExecutionFailed of { message: string }
  | OutputsNotCreated of { missing: Path.t list }
  | DependenciesFailed of { failed: Graph.SimpleGraph.Node_id.t list }

type action_status = Action_executor.action_status =
  | Cached of Std.Crypto.hash
  | Executed
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

type t = {
  completed: completed_action list;
}

let run = fun ~action_graph ~sandbox ~store ~session_id toolchain ~concurrency ->
  let low_level_result =
    Action_executor.execute
      ~action_graph
      ~sandbox
      ~store
      ~session_id
      toolchain
      ~concurrency
  in
  let completed =
    Action_graph.nodes action_graph
    |> List.filter_map ~fn:(fun (node: Action_node.t) ->
      match HashMap.get low_level_result.completed ~key:node.id with
      | Some result -> Some { node; result }
      | None -> None)
  in
  { completed }

let results = fun (result: t) -> result.completed

let rec find_first_map = fun items ~fn ->
  match items with
  | [] -> None
  | item :: rest -> (
      match fn item with
      | Some _ as result -> result
      | None -> find_first_map rest ~fn
    )

let find_result = fun (result: t) (node: Action_node.t) ->
  find_first_map
    result.completed
    ~fn:(fun completed_action ->
      if Graph.SimpleGraph.Node_id.eq completed_action.node.id node.id then
        Some completed_action.result
      else
        None)

let first_failure = fun (result: t) ->
  find_first_map result.completed ~fn:(fun completed_action ->
    match completed_action.result.status with
    | Failed err -> Some err
    | Cached _
    | Executed
    | Skipped -> None)

let ocamlc_warnings = fun (result: t) ->
  let seen = HashSet.create () in
  result.completed |> List.fold_left ~acc:[]
    ~fn:(fun acc completed_action ->
      List.fold_left completed_action.result.ocamlc_warnings ~acc
        ~fn:(fun acc warning ->
          if HashSet.contains seen ~value:warning then
            acc
          else
            let _ = HashSet.insert seen ~value:warning in
            acc @ [ warning ]))
