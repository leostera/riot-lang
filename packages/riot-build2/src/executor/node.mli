type t = Work_node.t
type kind = Work_node.kind =
  | UserIntent of User_intent.t
  | Goal of Goal.t
  | ToolchainReady of Toolchain_ready.t
  | SourceAnalysis of Source_analysis.t
  | PackageArtifact of Goal.build_package
  | PackageFinalize of Goal.build_package
  | ModulePlan of Goal.build_package
  | ActionPlan of Goal.build_package
  | ModuleDependencies of Goal.build_package
  | OCamlInterface of Rule.ocaml_source
  | OCamlByteImplementation of Rule.ocaml_source
  | OCamlImplementation of Rule.ocaml_source
  | OCamlGenerated of Rule.ocaml_generated
  | CObject of Rule.c_object
  | OCamlArchive of Goal.build_package
  | OCamlLibrary of Action_execution.t
  | ActionExecution of Action_execution.t
type status = Work_node.status =
  | Unplanned
  | Planning
  | Parked
  | Ready
  | Running
  | Completed
  | Failed

val id: t -> Node_id.t

val kind: t -> kind

val status: t -> status

val dependents: t -> Node_id.t list
