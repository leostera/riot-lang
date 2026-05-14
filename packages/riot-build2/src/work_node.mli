open Std

module Node_id: sig
  type t

  val equal: t -> t -> bool

  val compare: t -> t -> Order.t

  val from_int: int -> t

  val to_int: t -> int

  val to_string: t -> string
end

type status =
  | Unplanned
  | Planning
  | Parked
  | Ready
  | Running
  | Completed
  | Failed
type execution_mode =
  | Virtual
  | Concrete
type module_ref = {
  package: Riot_model.Package_name.t option;
  scope: string option;
  name: string;
}
type source_ref = {
  package: Riot_model.Package_name.t option;
  path: Path.t;
}
type key =
  | Intent of User_intent.t
  | Package of Riot_model.Package_name.t
  | Module of module_ref
  | Source of source_ref
  | GoalKey of Goal.t
  | ToolchainReadyKey of Toolchain_ready.key
  | SourceAnalysisKey of Source_analysis.key
  | PackageArtifactKey of Goal.build_package
  | PackageFinalizeKey of Goal.build_package
  | ModulePlanKey of Goal.build_package
  | ActionPlanKey of Goal.build_package
  | ModuleDependenciesKey of Goal.build_package
  | OCamlInterfaceKey of Rule.ocaml_source
  | OCamlByteImplementationKey of Rule.ocaml_source
  | OCamlImplementationKey of Rule.ocaml_source
  | OCamlGeneratedKey of Rule.ocaml_generated
  | CObjectKey of Rule.c_object
  | OCamlArchiveKey of Goal.build_package
  | OCamlLibraryKey of Action_execution.ref_
  | ActionExecutionKey of Action_execution.ref_
type kind =
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
type t

val key_from_kind: kind -> key

val kind_from_key: key -> kind option

val create: id:Node_id.t -> ?key:key -> kind -> t

val user_intent: id:Node_id.t -> User_intent.t -> t

val goal: id:Node_id.t -> Goal.t -> t

val toolchain_ready: id:Node_id.t -> Toolchain_ready.t -> t

val source_analysis: id:Node_id.t -> Source_analysis.t -> t

val package_artifact: id:Node_id.t -> Goal.build_package -> t

val package_finalize: id:Node_id.t -> Goal.build_package -> t

val module_plan: id:Node_id.t -> Goal.build_package -> t

val action_plan: id:Node_id.t -> Goal.build_package -> t

val module_dependencies: id:Node_id.t -> Goal.build_package -> t

val ocaml_interface: id:Node_id.t -> Rule.ocaml_source -> t

val ocaml_byte_implementation: id:Node_id.t -> Rule.ocaml_source -> t

val ocaml_implementation: id:Node_id.t -> Rule.ocaml_source -> t

val ocaml_generated: id:Node_id.t -> Rule.ocaml_generated -> t

val c_object: id:Node_id.t -> Rule.c_object -> t

val ocaml_archive: id:Node_id.t -> Goal.build_package -> t

val ocaml_library: id:Node_id.t -> Action_execution.t -> t

val action_execution: id:Node_id.t -> Action_execution.t -> t

val id: t -> Node_id.t

val key: t -> key

val kind: t -> kind

val execution_mode: t -> execution_mode

val execution_mode_of_kind: kind -> execution_mode

val status: t -> status

val dependencies: t -> Node_id.t list

val dependents: t -> Node_id.t list

val pending_dependency_count: t -> int

val dependencies_ready: t -> bool

val mark_as_planning: t -> unit

val mark_as_parked: t -> unit

val mark_as_ready: t -> unit

val mark_as_running: t -> unit

val mark_as_completed: t -> unit

val mark_as_failed: t -> unit

val add_dependency: t -> Node_id.t -> bool

val add_dependent: t -> Node_id.t -> bool

val add_pending_dependencies: t -> int -> unit

val mark_dependency_completed: t -> int

val add_dependencies: t -> Node_id.t list -> unit

val add_dependents: t -> Node_id.t list -> unit
