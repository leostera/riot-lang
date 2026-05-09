type t = Work_node.t
type kind = Work_node.kind =
  | UserIntent of User_intent.t
  | Goal of Goal.t
  | PackageWork of Package_work.t
  | ToolchainReady of Toolchain_ready.t
  | SourceAnalysis of Source_analysis.t
  | ModulePlan of Package_work.build_library
  | PackageFinalize of Package_work.build_library
  | ActionExecution of Action_execution.t
type status = Work_node.status =
  | Pending
  | Running
  | Completed
  | Failed

val id: t -> Node_id.t

val kind: t -> kind

val status: t -> status

val dependents: t -> Node_id.t list
