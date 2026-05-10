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

let package_finalize_key = fun build -> Work_node.PackageFinalizeKey build

let intern_package_finalize = fun t build ->
  intern
    t
    ~key:(package_finalize_key build)
    ~make:(fun () -> Work_node.PackageFinalize build)

let module_plan_key = fun build -> Work_node.ModulePlanKey build

let intern_module_plan = fun t build ->
  intern
    t
    ~key:(module_plan_key build)
    ~make:(fun () -> Work_node.ModulePlan build)

let action_plan_key = fun build -> Work_node.ActionPlanKey build

let intern_action_plan = fun t build ->
  intern
    t
    ~key:(action_plan_key build)
    ~make:(fun () -> Work_node.ActionPlan build)

let module_dependencies_key = fun source -> Work_node.ModuleDependenciesKey source

let intern_module_dependencies = fun t source ->
  intern
    t
    ~key:(module_dependencies_key source)
    ~make:(fun () -> Work_node.ModuleDependencies source)

let ocaml_interface_key = fun source -> Work_node.OCamlInterfaceKey source

let intern_ocaml_interface = fun t source ->
  intern
    t
    ~key:(ocaml_interface_key source)
    ~make:(fun () -> Work_node.OCamlInterface source)

let ocaml_implementation_key = fun source -> Work_node.OCamlImplementationKey source

let intern_ocaml_implementation = fun t source ->
  intern
    t
    ~key:(ocaml_implementation_key source)
    ~make:(fun () -> Work_node.OCamlImplementation source)

let ocaml_generated_key = fun source -> Work_node.OCamlGeneratedKey source

let intern_ocaml_generated = fun t source ->
  intern
    t
    ~key:(ocaml_generated_key source)
    ~make:(fun () -> Work_node.OCamlGenerated source)

let c_object_key = fun c_object -> Work_node.CObjectKey c_object

let intern_c_object = fun t c_object ->
  intern
    t
    ~key:(c_object_key c_object)
    ~make:(fun () -> Work_node.CObject c_object)

let ocaml_archive_key = fun build -> Work_node.OCamlArchiveKey build

let intern_ocaml_archive = fun t build ->
  intern
    t
    ~key:(ocaml_archive_key build)
    ~make:(fun () -> Work_node.OCamlArchive build)

let action_execution_key = fun (action: Action_execution.t) ->
  Work_node.ActionExecutionKey action.ref_

let ocaml_library_key = fun (action: Action_execution.t) -> Work_node.OCamlLibraryKey action.ref_

let intern_ocaml_library = fun t (action: Action_execution.t) ->
  intern
    t
    ~key:(ocaml_library_key action)
    ~make:(fun () -> Work_node.OCamlLibrary action)

let intern_action_execution = fun t (action: Action_execution.t) ->
  intern
    t
    ~key:(action_execution_key action)
    ~make:(fun () -> Work_node.ActionExecution action)
