open Std

module Atomic = Sync.Atomic
module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  nodes_by_key: (Work_node.key, Work_node.t) ConcurrentHashMap.t;
  nodes_by_id: (Work_node.Node_id.t, Work_node.t) ConcurrentHashMap.t;
  next_id: int Atomic.t;
}

let create = fun ?(next_id = 0) ?(capacity = 4_096) () -> {
  nodes_by_key = ConcurrentHashMap.with_capacity ~size:capacity;
  nodes_by_id = ConcurrentHashMap.with_capacity ~size:capacity;
  next_id = Atomic.make next_id;
}

let find = fun t key -> ConcurrentHashMap.get t.nodes_by_key ~key

let find_by_id = fun t id -> ConcurrentHashMap.get t.nodes_by_id ~key:id

let register = fun t node ->
  let canonical =
    ConcurrentHashMap.compute
      t.nodes_by_key
      ~key:(Work_node.key node)
      ~fn:(fun current ->
        match current with
        | Some existing -> ConcurrentHashMap.Abort existing
        | None -> ConcurrentHashMap.Insert (node, node))
  in
  let _ = ConcurrentHashMap.insert t.nodes_by_id ~key:(Work_node.id canonical) ~value:canonical in
  canonical

let allocate_id = fun t -> Work_node.Node_id.from_int (Int.succ (Atomic.fetch_and_add t.next_id 1))

let intern = fun t ~key ~make ->
  match find t key with
  | Some existing -> existing
  | None ->
      let candidate = Work_node.create ~id:(allocate_id t) ~key (make ()) in
      let canonical =
        ConcurrentHashMap.compute
          t.nodes_by_key
          ~key
          ~fn:(fun current ->
            match current with
            | Some existing -> ConcurrentHashMap.Abort existing
            | None -> ConcurrentHashMap.Insert (candidate, candidate))
      in
      let _ =
        ConcurrentHashMap.insert t.nodes_by_id ~key:(Work_node.id canonical) ~value:canonical
      in
      canonical

let package_key = fun package -> Work_node.Package package

let intern_package = fun t package ~make -> intern t ~key:(package_key package) ~make

let find_package = fun t package -> find t (package_key package)

let module_key = fun ~package ~scope ~name -> Work_node.Module { package; scope; name }

let intern_module = fun t ~package ~scope ~name ~make ->
  intern
    t
    ~key:(module_key ~package ~scope ~name)
    ~make

let find_module = fun t ~package ~scope ~name -> find t (module_key ~package ~scope ~name)

let goal_key = fun action -> Work_node.GoalKey action

let intern_goal = fun t action ->
  intern
    t
    ~key:(goal_key action)
    ~make:(fun () -> Work_node.Goal action)

let find_goal = fun t action -> find t (goal_key action)

let toolchain_ready_key = fun toolchain -> Work_node.ToolchainReadyKey toolchain

let intern_toolchain_ready = fun t toolchain ->
  intern
    t
    ~key:(toolchain_ready_key toolchain)
    ~make:(fun () -> Work_node.ToolchainReady toolchain)

let source_analysis_key = fun source -> Work_node.SourceAnalysisKey source.Source_analysis.key

let intern_source_analysis = fun t source ->
  intern
    t
    ~key:(source_analysis_key source)
    ~make:(fun () -> Work_node.SourceAnalysis source)

let package_artifact_key = fun build -> Work_node.PackageArtifactKey build

let intern_package_artifact = fun t build ->
  intern
    t
    ~key:(package_artifact_key build)
    ~make:(fun () -> Work_node.PackageArtifact build)

let module_plan_key = fun build -> Work_node.ModulePlanKey build

let intern_module_plan = fun t build ->
  intern
    t
    ~key:(module_plan_key build)
    ~make:(fun () -> Work_node.ModulePlan build)

let action_execution_key = fun (action: Action_execution.t) ->
  Work_node.ActionExecutionKey action.ref_

let intern_action_execution = fun t (action: Action_execution.t) ->
  intern
    t
    ~key:(action_execution_key action)
    ~make:(fun () -> Work_node.ActionExecution action)
