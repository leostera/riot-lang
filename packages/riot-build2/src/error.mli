type t =
  | IntentPlanningFailed of { reason: string }
  | MissingPackageCatalog
  | MissingPackage of {
      package: Riot_model.Package_name.t;
      available: Riot_model.Package_name.t list;
    }
  | UnsupportedGoal of {
      goal: Goal.t;
    }
  | ExternalDependencyUnsupported of {
      package: Riot_model.Package_name.t;
      dependency: Riot_model.Package_name.t;
    }
  | ToolchainFailed of {
      target: Riot_model.Target.t;
      reason: string;
    }
  | SourceAnalysisFailed of {
      source: Std.Path.t;
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
      missing: Std.Path.t list;
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

val message: t -> string
