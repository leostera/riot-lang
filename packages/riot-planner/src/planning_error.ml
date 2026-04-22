open Std
open Std.Collections

type t =
  | CyclicDependency of { cycle: string list }
  | ScanFailed of { path: Path.t; reason: string }
  | DependencyAnalysisFailed of { reason: string }
  | GraphBuildFailed of { reason: string }
  | TargetDependsOnInternalLibraryModule of {
      target_name: string;
      source: Path.t;
      requested_module: string;
      internal_module: string;
      public_module: string
    }
  | TargetDependsOnNamespacedInternalLibraryModule of {
      target_name: string;
      source: Path.t;
      requested_module: string;
      internal_module: string;
      public_module: string
    }
  | TargetDependsOnOtherTargetRoot of {
      target_name: string;
      source: Path.t;
      requested_module: string;
      other_target_name: string;
      other_target_module: string;
      public_module: string
    }
  | Exception of { exn: exn }

let to_string = function
  | CyclicDependency { cycle } -> "Cyclic dependency detected: " ^ String.concat " -> " cycle
  | ScanFailed { path; reason } -> "Failed to scan " ^ Path.to_string path ^ ": " ^ reason
  | DependencyAnalysisFailed { reason } -> "Dependency analysis failed: " ^ reason
  | GraphBuildFailed { reason } -> "Graph build failed: " ^ reason
  | TargetDependsOnInternalLibraryModule {
    target_name;
    source;
    requested_module;
    internal_module;
    public_module
  } -> "Target '"
  ^ target_name
  ^ "' source '"
  ^ Path.to_string source
  ^ "' depends on internal library module '"
  ^ internal_module
  ^ "' via '"
  ^ requested_module
  ^ "'. Targets may only depend on the public package module '"
  ^ public_module
  ^ "'. Use '"
  ^ public_module
  ^ "."
  ^ requested_module
  ^ "' instead."
  | TargetDependsOnNamespacedInternalLibraryModule {
    target_name;
    source;
    requested_module;
    internal_module;
    public_module
  } -> "Target '"
  ^ target_name
  ^ "' source '"
  ^ Path.to_string source
  ^ "' depends on namespaced internal library module '"
  ^ internal_module
  ^ "' via '"
  ^ requested_module
  ^ "'. Namespaced internal modules are not public. Use '"
  ^ public_module
  ^ "."
  ^ (internal_module
  |> String.split ~by:"__"
  |> List.reverse
  |> List.head
  |> Option.unwrap_or ~default:requested_module)
  ^ "' instead."
  | TargetDependsOnOtherTargetRoot {
    target_name;
    source;
    requested_module;
    other_target_name;
    other_target_module;
    public_module
  } -> "Target '"
  ^ target_name
  ^ "' source '"
  ^ Path.to_string source
  ^ "' depends on target root module '"
  ^ other_target_module
  ^ "' via '"
  ^ requested_module
  ^ "' from target '"
  ^ other_target_name
  ^ "'. Target roots are private entrypoints. Move shared code behind the public package module '"
  ^ public_module
  ^ "' or a shared helper module."
  | Exception { exn } -> "Unexpected exception: " ^ Kernel.Exception.to_string exn

let to_json = function
  | CyclicDependency { cycle } -> Data.Json.obj
    [
      ("type", Data.Json.string "cyclic_dependency");
      ("cycle", Data.Json.array (List.map cycle ~fn:Data.Json.string))
    ]
  | ScanFailed { path; reason } -> Data.Json.obj
    [
      ("type", Data.Json.string "scan_failed");
      ("path", Data.Json.string (Path.to_string path));
      ("reason", Data.Json.string reason)
    ]
  | DependencyAnalysisFailed { reason } -> Data.Json.obj
    [ ("type", Data.Json.string "dependency_analysis_failed"); ("reason", Data.Json.string reason) ]
  | GraphBuildFailed { reason } -> Data.Json.obj
    [ ("type", Data.Json.string "graph_build_failed"); ("reason", Data.Json.string reason) ]
  | TargetDependsOnInternalLibraryModule {
    target_name;
    source;
    requested_module;
    internal_module;
    public_module
  } -> Data.Json.obj
    [
      ("type", Data.Json.string "target_depends_on_internal_library_module");
      ("target_name", Data.Json.string target_name);
      ("source", Data.Json.string (Path.to_string source));
      ("requested_module", Data.Json.string requested_module);
      ("internal_module", Data.Json.string internal_module);
      ("public_module", Data.Json.string public_module)
    ]
  | TargetDependsOnNamespacedInternalLibraryModule {
    target_name;
    source;
    requested_module;
    internal_module;
    public_module
  } -> Data.Json.obj
    [
      ("type", Data.Json.string "target_depends_on_namespaced_internal_library_module");
      ("target_name", Data.Json.string target_name);
      ("source", Data.Json.string (Path.to_string source));
      ("requested_module", Data.Json.string requested_module);
      ("internal_module", Data.Json.string internal_module);
      ("public_module", Data.Json.string public_module)
    ]
  | TargetDependsOnOtherTargetRoot {
    target_name;
    source;
    requested_module;
    other_target_name;
    other_target_module;
    public_module
  } -> Data.Json.obj
    [
      ("type", Data.Json.string "target_depends_on_other_target_root");
      ("target_name", Data.Json.string target_name);
      ("source", Data.Json.string (Path.to_string source));
      ("requested_module", Data.Json.string requested_module);
      ("other_target_name", Data.Json.string other_target_name);
      ("other_target_module", Data.Json.string other_target_module);
      ("public_module", Data.Json.string public_module)
    ]
  | Exception { exn } -> Data.Json.obj
    [
      ("type", Data.Json.string "exception");
      ("message", Data.Json.string (Kernel.Exception.to_string exn))
    ]
