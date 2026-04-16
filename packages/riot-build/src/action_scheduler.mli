open Std
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

type t

val run:
  action_graph:Action_graph.t ->
  sandbox:Sandbox.t ->
  store:Riot_store.Store.t ->
  session_id:Riot_model.Session_id.t ->
  Riot_toolchain.t ->
  concurrency:int ->
  t

val results: t -> completed_action list

val find_result: t -> Action_node.t -> execution_result option

val first_failure: t -> action_error option

val ocamlc_warnings: t -> string list
