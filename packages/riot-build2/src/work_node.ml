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
  | Pending
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
  | PackageWorkKey of Package_work.t
  | ToolchainReadyKey of Toolchain_ready.key
  | SourceAnalysisKey of Source_analysis.key
  | ModulePlanKey of Package_work.build_library
  | PackageFinalizeKey of Package_work.build_library
  | ActionExecutionKey of Action_execution.ref_

type kind =
  | UserIntent of User_intent.t
  | Goal of Goal.t
  | PackageWork of Package_work.t
  | ToolchainReady of Toolchain_ready.t
  | SourceAnalysis of Source_analysis.t
  | ModulePlan of Package_work.build_library
  | PackageFinalize of Package_work.build_library
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
  | PackageWork work -> PackageWorkKey work
  | ToolchainReady toolchain -> ToolchainReadyKey toolchain
  | SourceAnalysis source -> SourceAnalysisKey source.key
  | ModulePlan build -> ModulePlanKey build
  | PackageFinalize build -> PackageFinalizeKey build
  | ActionExecution action -> ActionExecutionKey action.ref_

let kind_from_key = fun __tmp1 ->
  match __tmp1 with
  | Intent intent -> Some (UserIntent intent)
  | GoalKey goal -> Some (Goal goal)
  | PackageWorkKey work -> Some (PackageWork work)
  | ToolchainReadyKey toolchain -> Some (ToolchainReady toolchain)
  | ModulePlanKey build -> Some (ModulePlan build)
  | PackageFinalizeKey build -> Some (PackageFinalize build)
  | Package _
  | Module _
  | Source _
  | SourceAnalysisKey _
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
    status = Atomic.make Pending;
    dependencies = ConcurrentHashMap.with_capacity ~size:16;
    dependents = ConcurrentHashMap.with_capacity ~size:16;
    pending_dependencies = Atomic.make 0;
  }

let user_intent = fun ~id intent -> create ~id (UserIntent intent)

let goal = fun ~id goal -> create ~id (Goal goal)

let package_work = fun ~id work -> create ~id (PackageWork work)

let toolchain_ready = fun ~id toolchain -> create ~id (ToolchainReady toolchain)

let source_analysis = fun ~id source -> create ~id (SourceAnalysis source)

let module_plan = fun ~id build -> create ~id (ModulePlan build)

let package_finalize = fun ~id build -> create ~id (PackageFinalize build)

let action_execution = fun ~id action -> create ~id (ActionExecution action)

let id = fun node -> node.id

let key = fun node -> node.key

let kind = fun node -> node.kind

let execution_mode_of_kind = fun __tmp1 ->
  match __tmp1 with
  | UserIntent _
  | Goal _
  | PackageWork _ -> Virtual
  | ToolchainReady _
  | SourceAnalysis _
  | ModulePlan _
  | PackageFinalize _
  | ActionExecution _ -> Concrete

let execution_mode = fun node -> execution_mode_of_kind node.kind

let status = fun node -> Atomic.get node.status

let status_to_string = fun __tmp1 ->
  match __tmp1 with
  | Pending -> "Pending"
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
  | (Pending, Running)
  | (Running, Pending)
  | (Pending, Completed)
  | (Running, Completed)
  | (Pending, Failed)
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

let mark_as_running = fun node -> transition node ~to_:Running

let mark_as_pending = fun node -> transition node ~to_:Pending

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
