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
type t = {
  completed_actions: completed_action list;
  first_failure: action_error option;
  ocamlc_warnings: string list;
}

val summarize_completed:
  action_graph:Action_graph.t ->
  completed_results:(Graph.SimpleGraph.Node_id.t, execution_result) HashMap.t ->
  t

val run:
  action_graph:Action_graph.t ->
  sandbox:Sandbox.t ->
  store:Riot_store.Store.t ->
  session_id:Riot_model.Session_id.t ->
  Riot_toolchain.t ->
  concurrency:int ->
  t

val find_result: t -> Action_node.t -> execution_result option
