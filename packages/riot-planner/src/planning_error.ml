open Std
open Std.Collections

type executable_main_error =
  | MissingMain
  | MultipleMainDefinitions of { count: int }
  | InvalidMainParameters of { parameters: string list }

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
    public_module: string;
  }
  | TargetDependsOnNamespacedInternalLibraryModule of {
    target_name: string;
    source: Path.t;
    requested_module: string;
    internal_module: string;
    public_module: string;
  }
  | TargetDependsOnOtherTargetRoot of {
    target_name: string;
    source: Path.t;
    requested_module: string;
    other_target_name: string;
    other_target_module: string;
    public_module: string;
  }
  | SourceDependsOnUndeclaredPackageModule of {
    package_name: string;
    source: Path.t;
    requested_module: string;
    allowed_modules: string list;
  }
  | InvalidExecutableMain of {
    package_name: string;
    target_name: string;
    source: Path.t;
    file: Path.t;
    error: executable_main_error;
  }
  | Exception of { exn: exn }

let executable_main_error_to_string = function
  | MissingMain -> "it does not define a top-level `let main ~args = ...` binding"
  | MultipleMainDefinitions { count } -> "it defines " ^ Int.to_string count ^ " top-level `main` bindings; executable entrypoints must define exactly one"
  | InvalidMainParameters { parameters } ->
      let parameter_list =
        match parameters with
        | [] -> "<none>"
        | _ -> String.concat ", " parameters
      in
      "`main` must be written with exactly one labeled `~args` parameter, for example `let main ~args = Ok ()`; found parameters: " ^ parameter_list

let to_string = function
  | CyclicDependency { cycle } -> "Cyclic dependency detected: " ^ String.concat " -> " cycle
  | ScanFailed { path; reason } -> "Failed to scan " ^ Path.to_string path ^ ": " ^ reason
  | DependencyAnalysisFailed { reason } -> "Dependency analysis failed: " ^ reason
  | GraphBuildFailed { reason } -> "Graph build failed: " ^ reason
  | TargetDependsOnInternalLibraryModule { target_name; source; requested_module; internal_module; public_module } -> "Target '" ^ target_name ^ "' source '" ^ Path.to_string source ^ "' depends on internal library module '" ^ internal_module ^ "' via '" ^ requested_module ^ "'. Targets may only depend on the public package module '" ^ public_module ^ "'. Use '" ^ public_module ^ "." ^ requested_module ^ "' instead."
  | TargetDependsOnNamespacedInternalLibraryModule { target_name; source; requested_module; internal_module; public_module } -> "Target '" ^ target_name ^ "' source '" ^ Path.to_string source ^ "' depends on namespaced internal library module '" ^ internal_module ^ "' via '" ^ requested_module ^ "'. Namespaced internal modules are not public. Use '" ^ public_module ^ "." ^ (internal_module |> String.split ~by:"__" |> List.reverse |> List.head |> Option.unwrap_or ~default:requested_module) ^ "' instead."
  | TargetDependsOnOtherTargetRoot { target_name; source; requested_module; other_target_name; other_target_module; public_module } -> "Target '" ^ target_name ^ "' source '" ^ Path.to_string source ^ "' depends on target root module '" ^ other_target_module ^ "' via '" ^ requested_module ^ "' from target '" ^ other_target_name ^ "'. Target roots are private entrypoints. Move shared code behind the public package module '" ^ public_module ^ "' or a shared helper module."
  | SourceDependsOnUndeclaredPackageModule { package_name; source; requested_module; allowed_modules } -> "Package '" ^ package_name ^ "' source '" ^ Path.to_string source ^ "' depends on module '" ^ requested_module ^ "', but that module is not provided by this package or one of its direct dependencies. Allowed package modules: " ^ String.concat ", " allowed_modules ^ "."
  | InvalidExecutableMain { package_name; target_name; source; file; error } -> "Package '" ^ package_name ^ "' executable target '" ^ target_name ^ "' source '" ^ Path.to_string source ^ "' file '" ^ Path.to_string file ^ "' has an invalid entrypoint: " ^ executable_main_error_to_string error ^ "."
  | Exception { exn } -> "Unexpected exception: " ^ Exception.to_string exn

let executable_main_error_to_json = function
  | MissingMain ->
      Data.Json.obj
        [
          "type", Data.Json.string "missing_main";
        ]
  | MultipleMainDefinitions { count } ->
      Data.Json.obj
        [
          "type", Data.Json.string "multiple_main_definitions";
          "count", Data.Json.Int count;
        ]
  | InvalidMainParameters { parameters } ->
      Data.Json.obj
        [
          "type", Data.Json.string "invalid_main_parameters";
          "parameters", Data.Json.array (List.map parameters ~fn:Data.Json.string);
        ]

let to_json = function
  | CyclicDependency { cycle } ->
      Data.Json.obj
        [
          "type", Data.Json.string "cyclic_dependency";
          "cycle", Data.Json.array (List.map cycle ~fn:Data.Json.string);
        ]
  | ScanFailed { path; reason } ->
      Data.Json.obj
        [
          "type", Data.Json.string "scan_failed";
          "path", Data.Json.string (Path.to_string path);
          "reason", Data.Json.string reason;
        ]
  | DependencyAnalysisFailed { reason } ->
      Data.Json.obj
        [
          "type", Data.Json.string "dependency_analysis_failed";
          "reason", Data.Json.string reason;
        ]
  | GraphBuildFailed { reason } ->
      Data.Json.obj
        [
          "type", Data.Json.string "graph_build_failed";
          "reason", Data.Json.string reason;
        ]
  | TargetDependsOnInternalLibraryModule { target_name; source; requested_module; internal_module; public_module } ->
      Data.Json.obj
        [
          "type", Data.Json.string "target_depends_on_internal_library_module";
          "target_name", Data.Json.string target_name;
          "source", Data.Json.string (Path.to_string source);
          "requested_module", Data.Json.string requested_module;
          "internal_module", Data.Json.string internal_module;
          "public_module", Data.Json.string public_module;
        ]
  | TargetDependsOnNamespacedInternalLibraryModule { target_name; source; requested_module; internal_module; public_module } ->
      Data.Json.obj
        [
          "type", Data.Json.string "target_depends_on_namespaced_internal_library_module";
          "target_name", Data.Json.string target_name;
          "source", Data.Json.string (Path.to_string source);
          "requested_module", Data.Json.string requested_module;
          "internal_module", Data.Json.string internal_module;
          "public_module", Data.Json.string public_module;
        ]
  | TargetDependsOnOtherTargetRoot { target_name; source; requested_module; other_target_name; other_target_module; public_module } ->
      Data.Json.obj
        [
          "type", Data.Json.string "target_depends_on_other_target_root";
          "target_name", Data.Json.string target_name;
          "source", Data.Json.string (Path.to_string source);
          "requested_module", Data.Json.string requested_module;
          "other_target_name", Data.Json.string other_target_name;
          "other_target_module", Data.Json.string other_target_module;
          "public_module", Data.Json.string public_module;
        ]
  | SourceDependsOnUndeclaredPackageModule { package_name; source; requested_module; allowed_modules } ->
      Data.Json.obj
        [
          "type", Data.Json.string "source_depends_on_undeclared_package_module";
          "package_name", Data.Json.string package_name;
          "source", Data.Json.string (Path.to_string source);
          "requested_module", Data.Json.string requested_module;
          "allowed_modules", Data.Json.array (List.map allowed_modules ~fn:Data.Json.string);
        ]
  | InvalidExecutableMain { package_name; target_name; source; file; error } ->
      Data.Json.obj
        [
          "type", Data.Json.string "invalid_executable_main";
          "package_name", Data.Json.string package_name;
          "target_name", Data.Json.string target_name;
          "source", Data.Json.string (Path.to_string source);
          "file", Data.Json.string (Path.to_string file);
          "error", executable_main_error_to_json error;
        ]
  | Exception { exn } ->
      Data.Json.obj
        [
          "type", Data.Json.string "exception";
          "message", Data.Json.string (Exception.to_string exn);
        ]
