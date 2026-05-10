open Std

module Atomic = Sync.Atomic
module ConcurrentHashMap = Collections.ConcurrentHashMap

module Node_id = struct
  type t = int

  let equal = Int.equal

  let compare = Int.compare

  let from_int = fun id -> id

  let to_int = fun id -> id

  let to_string = Int.to_string
end

type status =
  | Unplanned
  | Planning
  | Waiting
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
  | OCamlLibraryKey of Action_execution.ref_
  | OCamlArchiveKey of Action_execution.ref_
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
  | OCamlLibrary of Action_execution.t
  | OCamlArchive of Action_execution.t
  | ActionExecution of Action_execution.t

type t = {
  id: Node_id.t;
  key: key;
  kind: kind;
  status: status Atomic.t;
  dependencies: (Node_id.t, unit) ConcurrentHashMap.t;
  dependents: (Node_id.t, unit) ConcurrentHashMap.t;
  pending_dependencies: int Atomic.t;
}

let key_from_kind = fun __tmp1 ->
  match __tmp1 with
  | UserIntent intent -> Intent intent
  | Goal goal -> GoalKey goal
  | ToolchainReady toolchain -> ToolchainReadyKey toolchain
  | SourceAnalysis source -> SourceAnalysisKey source.key
  | PackageArtifact build -> PackageArtifactKey build
  | PackageFinalize build -> PackageFinalizeKey build
  | ModulePlan build -> ModulePlanKey build
  | ActionPlan build -> ActionPlanKey build
  | OCamlLibrary action -> OCamlLibraryKey action.ref_
  | OCamlArchive action -> OCamlArchiveKey action.ref_
  | ActionExecution action -> ActionExecutionKey action.ref_

let kind_from_key = fun __tmp1 ->
  match __tmp1 with
  | Intent intent -> Some (UserIntent intent)
  | GoalKey goal -> Some (Goal goal)
  | ToolchainReadyKey toolchain -> Some (ToolchainReady toolchain)
  | PackageArtifactKey build -> Some (PackageArtifact build)
  | PackageFinalizeKey build -> Some (PackageFinalize build)
  | ModulePlanKey build -> Some (ModulePlan build)
  | ActionPlanKey build -> Some (ActionPlan build)
  | Package _
  | Module _
  | Source _
  | SourceAnalysisKey _
  | OCamlLibraryKey _
  | OCamlArchiveKey _
  | ActionExecutionKey _ -> None

let create = fun ~id ?key kind ->
  let key =
    key
    |> Option.unwrap_or_else ~fn:(fun () -> key_from_kind kind)
  in
  {
    id;
    key;
    kind;
    status = Atomic.make Unplanned;
    dependencies = ConcurrentHashMap.with_capacity ~size:16;
    dependents = ConcurrentHashMap.with_capacity ~size:16;
    pending_dependencies = Atomic.make 0;
  }

let user_intent = fun ~id intent -> create ~id (UserIntent intent)

let goal = fun ~id goal -> create ~id (Goal goal)

let toolchain_ready = fun ~id toolchain -> create ~id (ToolchainReady toolchain)

let source_analysis = fun ~id source -> create ~id (SourceAnalysis source)

let package_artifact = fun ~id build -> create ~id (PackageArtifact build)

let package_finalize = fun ~id build -> create ~id (PackageFinalize build)

let module_plan = fun ~id build -> create ~id (ModulePlan build)

let action_plan = fun ~id build -> create ~id (ActionPlan build)

let ocaml_library = fun ~id action -> create ~id (OCamlLibrary action)

let ocaml_archive = fun ~id action -> create ~id (OCamlArchive action)

let action_execution = fun ~id action -> create ~id (ActionExecution action)

let id = fun node -> node.id

let key = fun node -> node.key

let kind = fun node -> node.kind

let execution_mode_of_kind = fun __tmp1 ->
  match __tmp1 with
  | UserIntent _
  | Goal (Goal.BuildPackage _) -> Virtual
  | Goal _
  | ToolchainReady _
  | SourceAnalysis _
  | PackageArtifact _
  | PackageFinalize _
  | ModulePlan _
  | ActionPlan _
  | OCamlLibrary _
  | OCamlArchive _
  | ActionExecution _ -> Concrete

let execution_mode = fun node -> execution_mode_of_kind node.kind

let status = fun node -> Atomic.get node.status

let status_to_string = fun __tmp1 ->
  match __tmp1 with
  | Unplanned -> "Unplanned"
  | Planning -> "Planning"
  | Waiting -> "Waiting"
  | Ready -> "Ready"
  | Running -> "Running"
  | Completed -> "Completed"
  | Failed -> "Failed"

let invalid_transition_message = fun node ~from ~to_ ->
  "invalid work node transition for node "
  ^ Node_id.to_string node.id
  ^ ": "
  ^ status_to_string from
  ^ " -> "
  ^ status_to_string to_

let valid_transition = fun ~from ~to_ ->
  match (from, to_) with
  | (Unplanned, Planning)
  | (Unplanned, Waiting)
  | (Unplanned, Ready)
  | (Unplanned, Failed)
  | (Planning, Waiting)
  | (Planning, Ready)
  | (Planning, Failed)
  | (Waiting, Ready)
  | (Waiting, Failed)
  | (Ready, Running)
  | (Ready, Completed)
  | (Ready, Failed)
  | (Running, Waiting)
  | (Running, Ready)
  | (Running, Completed)
  | (Running, Failed) -> true
  | _ -> false

let transition = fun node ~to_ ->
  let rec loop () =
    let from = Atomic.get node.status in
    if valid_transition ~from ~to_ then
      if Atomic.compare_and_set node.status from to_ then
        ()
      else
        loop ()
    else
      panic (invalid_transition_message node ~from ~to_)
  in
  loop ()

let dependencies = fun node -> ConcurrentHashMap.keys node.dependencies

let dependents = fun node -> ConcurrentHashMap.keys node.dependents

let pending_dependency_count = fun node -> Atomic.get node.pending_dependencies

let dependencies_ready = fun node -> Int.equal (pending_dependency_count node) 0

let mark_as_planning = fun node -> transition node ~to_:Planning

let mark_as_waiting = fun node -> transition node ~to_:Waiting

let mark_as_ready = fun node -> transition node ~to_:Ready

let mark_as_running = fun node -> transition node ~to_:Running

let mark_as_completed = fun node -> transition node ~to_:Completed

let mark_as_failed = fun node -> transition node ~to_:Failed

let add_id = fun ids id ->
  ConcurrentHashMap.compute
    ids
    ~key:id
    ~fn:(fun current ->
      match current with
      | Some () -> ConcurrentHashMap.Abort false
      | None -> ConcurrentHashMap.Insert ((), true))

let add_dependency = fun node dependency -> add_id node.dependencies dependency

let add_dependent = fun node dependent -> add_id node.dependents dependent

let add_pending_dependencies = fun node count ->
  if count > 0 then
    let _ = Atomic.fetch_and_add node.pending_dependencies count in
    ()

let mark_dependency_completed = fun node ->
  let rec loop () =
    let current = Atomic.get node.pending_dependencies in
    if current <= 0 then
      0
    else
      let next = current - 1 in
      if Atomic.compare_and_set node.pending_dependencies current next then
        next
      else
        loop ()
  in
  loop ()

let add_dependencies = fun node new_dependencies ->
  List.for_each new_dependencies ~fn:(fun dependency ->
    ignore (add_dependency node dependency))

let add_dependents = fun node new_dependents ->
  List.for_each new_dependents ~fn:(fun dependent ->
    ignore (add_dependent node dependent))
