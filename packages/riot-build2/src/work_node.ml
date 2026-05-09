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

let status = fun node -> Atomic.get node.status

let dependencies = fun node -> ConcurrentHashMap.keys node.dependencies

let dependents = fun node -> ConcurrentHashMap.keys node.dependents

let pending_dependency_count = fun node -> Atomic.get node.pending_dependencies

let dependencies_ready = fun node -> Int.equal (pending_dependency_count node) 0

let set_status = fun node status -> Atomic.set node.status status

let compare_and_set_status = fun node ~from ~to_ ->
  Atomic.compare_and_set node.status from to_

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
