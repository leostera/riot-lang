type t = Work_node.t
type kind = Work_node.kind =
  | UserIntent of User_intent.t
  | Goal of Goal.t
  | ToolchainReady of Toolchain_ready.t
  | SourceAnalysis of Source_analysis.t
  | PackageArtifact of Goal.build_package
  | ModulePlan of Goal.build_package
  | ActionExecution of Action_execution.t
type status = Work_node.status =
  | Unplanned
  | Waiting
  | Ready
  | Running
  | Completed
  | Failed

val id: t -> Node_id.t

val kind: t -> kind

val status: t -> status

val dependents: t -> Node_id.t list
