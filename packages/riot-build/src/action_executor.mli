open Std
open Std.Collections
open Riot_planner

module G = Graph.SimpleGraph

(** Errors that can occur during action execution *)
type action_error =
  | ExecutionFailed of { message: string }
  | OutputsNotCreated of {
      missing: Path.t list;
    }
  | DependenciesFailed of {
      failed: G.Node_id.t list;
    }
(** Status of an executed action *)
type action_status =
  | Cached of Riot_store.Artifact.t
  | Executed of Riot_store.Artifact.t
  | Failed of action_error
  | Skipped
(** Result of executing a single action *)
type execution_result = {
  node_id: G.Node_id.t;
  status: action_status;
  ocamlc_warnings: string list;
  duration: Time.Duration.t;
  started_at: Time.Instant.t;
  completed_at: Time.Instant.t;
}

(**
   Execute a single planned action node.

   This low-level primitive is used by higher-level schedulers that need
   explicit control over global readiness/dispatch policy (for example,
   workspace-level action scheduling).

   [completed] is the dependency result table for the action graph this node
   belongs to; it is consulted to implement dependency-failure skipping.

   Source staging supports both package-relative and workspace-relative source
   paths to stay compatible with serialized plans loaded from cache.
*)
val execute_node:
  completed:(G.Node_id.t, execution_result) HashMap.t ->
  store:Riot_store.Store.t ->
  session_id:Riot_model.Session_id.t ->
  build_target:Riot_model.Target.t ->
  Riot_toolchain.t ->
  Path.t ->
  Action_node.t ->
  execution_result

val compute_action_input_hash:
  planned_hash:Crypto.hash ->
  dependency_output_hashes:Crypto.hash list ->
  Crypto.hash
