type t = Work_node.t

type kind = Work_node.kind =
  | UserIntent of User_intent.t
  | Goal of Goal.t
  | ToolchainReady of Toolchain_ready.t
  | SourceAnalysis of Source_analysis.t
  | ModulePlan of Goal.build_package
  | ActionExecution of Action_execution.t

type status = Work_node.status =
  | Pending
  | Running
  | Completed
  | Failed

let id = Work_node.id

let kind = Work_node.kind

let status = Work_node.status

let dependents = Work_node.dependents
