open Std

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
  | Exception of { exn: exn }
val to_string: t -> string

val to_json: t -> Std.Data.Json.t
