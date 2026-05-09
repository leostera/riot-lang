open Std

type t =
  | IntentPlanningFailed of { reason: string }
  | MissingPackageCatalog
  | MissingPackage of {
      package: Riot_model.Package_name.t;
      available: Riot_model.Package_name.t list;
    }
  | UnsupportedGoal of { goal: Goal.t }
  | ExternalDependencyUnsupported of {
      package: Riot_model.Package_name.t;
      dependency: Riot_model.Package_name.t;
    }
  | ToolchainFailed of {
      target: Riot_model.Target.t;
      reason: string;
    }
  | SourceAnalysisFailed of {
      source: Path.t;
      reason: string;
    }
  | ModulePlanningFailed of {
      package: Riot_model.Package_name.t;
      reason: string;
    }
  | ActionExecutionFailed of {
      package: Riot_model.Package_name.t;
      reason: string;
    }
  | ActionOutputsNotCreated of {
      package: Riot_model.Package_name.t;
      missing: Path.t list;
    }
  | StoreFailed of {
      package: Riot_model.Package_name.t option;
      reason: string;
    }
  | GraphCacheEncodeFailed of {
      namespace: Riot_store.Store.node_payload_namespace;
      reason: string;
    }
  | GraphCacheDecodeFailed of {
      namespace: Riot_store.Store.node_payload_namespace;
      reason: string;
    }
  | DependencyFailed of {
      node: Work_node.Node_id.t;
      dependency: Work_node.Node_id.t;
    }
  | WorkerFailed of { message: string }
  | ExecutorInvariantViolated of { message: string }

let message = fun __tmp1 ->
  match __tmp1 with
  | IntentPlanningFailed { reason } -> reason
  | MissingPackageCatalog -> "package catalog is required for this work graph"
  | MissingPackage { package; available } ->
      "package "
      ^ Riot_model.Package_name.to_string package
      ^ " was not found; available packages: "
      ^ String.concat ", " (List.map available ~fn:Riot_model.Package_name.to_string)
  | UnsupportedGoal _ -> "goal is not supported by this build2 slice"
  | ExternalDependencyUnsupported { package; dependency } ->
      "package "
      ^ Riot_model.Package_name.to_string package
      ^ " depends on external package "
      ^ Riot_model.Package_name.to_string dependency
      ^ ", which is not supported by this build2 slice"
  | ToolchainFailed { target; reason } ->
      "toolchain for " ^ Riot_model.Target.to_string target ^ " failed: " ^ reason
  | SourceAnalysisFailed { source; reason } ->
      "source analysis failed for " ^ Path.to_string source ^ ": " ^ reason
  | ModulePlanningFailed { package; reason } ->
      "module planning failed for " ^ Riot_model.Package_name.to_string package ^ ": " ^ reason
  | ActionExecutionFailed { package; reason } ->
      "action execution failed for " ^ Riot_model.Package_name.to_string package ^ ": " ^ reason
  | ActionOutputsNotCreated { package; missing } ->
      "action outputs were not created for "
      ^ Riot_model.Package_name.to_string package
      ^ ": "
      ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | StoreFailed { package; reason } ->
      let package_label =
        match package with
        | Some package -> Riot_model.Package_name.to_string package
        | None -> "workspace"
      in
      "store operation failed for " ^ package_label ^ ": " ^ reason
  | GraphCacheEncodeFailed { namespace; reason } ->
      "failed to encode "
      ^ Riot_store.Store.node_payload_namespace_to_string namespace
      ^ " graph cache payload: "
      ^ reason
  | GraphCacheDecodeFailed { namespace; reason } ->
      "failed to decode "
      ^ Riot_store.Store.node_payload_namespace_to_string namespace
      ^ " graph cache payload: "
      ^ reason
  | DependencyFailed { node; dependency } ->
      "work node "
      ^ Work_node.Node_id.to_string node
      ^ " depends on failed work node "
      ^ Work_node.Node_id.to_string dependency
  | WorkerFailed { message } -> message
  | ExecutorInvariantViolated { message } -> message
