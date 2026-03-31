open Std
open Std.Collections

type t =
  | CyclicDependency of {
      cycle : string list;
    }
  | ScanFailed of {
      path : Path.t;
      reason : string;
    }
  | DependencyAnalysisFailed of {
      reason : string;
    }
  | GraphBuildFailed of {
      reason : string;
    }
  | Exception of {
      exn : exn;
    }

let to_string =
  function
  | CyclicDependency { cycle } -> "Cyclic dependency detected: " ^ String.concat " -> " cycle
  | ScanFailed { path; reason } -> "Failed to scan " ^ Path.to_string path ^ ": " ^ reason
  | DependencyAnalysisFailed { reason } -> "Dependency analysis failed: " ^ reason
  | GraphBuildFailed { reason } -> "Graph build failed: " ^ reason
  | Exception { exn } -> "Unexpected exception: " ^ Exception.to_string exn

let to_json =
  function
  | CyclicDependency { cycle } -> Data.Json.obj
  [
    ("type", Data.Json.string "cyclic_dependency");
    ("cycle", Data.Json.array (List.map Data.Json.string cycle))
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
  | Exception { exn } -> Data.Json.obj
  [ ("type", Data.Json.string "exception"); ("message", Data.Json.string (Exception.to_string exn)) ]
