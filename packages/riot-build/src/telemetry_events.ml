open Std
open Std.Collections
open Riot_model
open Riot_planner
open Riot_store

(** Error types for package builds *)
type package_error =
  | PlanningFailed of Planning_error.t
  | ExecutionFailed of { message: string }
  | ActionExecutionFailed of { message: string }
  | ActionOutputsNotCreated of {
      missing: Path.t list;
    }
  | ActionDependenciesFailed of {
      failed: Graph.SimpleGraph.Node_id.t list;
    }

type package_planning_status = [ | `Planned | `MissingDependencies | `FailedDependencies | `Failed]

type package_planning_breakdown = {
  dependency_count: int;
  dependency_check_duration: Time.Duration.t;
  input_hash_duration: Time.Duration.t;
  artifact_lookup_duration: Time.Duration.t;
  artifact_cache_hit: bool;
  plan_bundle_lookup_duration: Time.Duration.t;
  plan_bundle_decode_duration: Time.Duration.t;
  plan_bundle_cache_hit: bool;
  module_plan_duration: Time.Duration.t;
}

type workspace_graph_breakdown = {
  build_node_realization_count: int;
  build_node_realization_duration: Time.Duration.t;
  runtime_node_realization_count: int;
  runtime_node_realization_duration: Time.Duration.t;
  dev_node_realization_count: int;
  dev_node_realization_duration: Time.Duration.t;
  edge_wiring_duration: Time.Duration.t;
}

type warning_source = [ | `Fresh | `Cached]

type Telemetry.event +=
  | PackageStarted of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      started_at: Time.Instant.t;
    }
  | WorkspacePlanStarted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      workspace_package_count: int;
    }
  | WorkspacePlanCompleted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      workspace_package_count: int;
      planned_package_count: int;
      duration: Time.Duration.t;
    }
  | WorkspaceManifestFilterCompleted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      filtered_workspace_package_count: int;
      duration: Time.Duration.t;
    }
  | WorkspaceGraphCreated of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      node_count: int;
      breakdown: workspace_graph_breakdown;
      duration: Time.Duration.t;
    }
  | WorkspaceTargetGraphFiltered of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      node_count: int;
      duration: Time.Duration.t;
    }
  | WorkspaceTopologicalSortCompleted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      sorted_package_count: int;
      duration: Time.Duration.t;
    }
  | PlanningWorkspaceStarted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      package_count: int;
    }
  | PlanningWorkspaceCompleted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      duration: Time.Duration.t;
      planned_count: int;
      missing_count: int;
      failed_count: int;
    }
  | PackagePlanningResult of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      status: package_planning_status;
      duration: Time.Duration.t;
      reason: string option;
    }
  | PackagePlanningBreakdown of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      breakdown: package_planning_breakdown;
    }
  | CompilationStarted of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      action_count: int;
      started_at: Time.Instant.t;
    }
  | SandboxCreated of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      path: Path.t;
      created_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | SandboxInputsCopied of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      input_count: int;
      copied_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | SandboxDependenciesCopied of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      dependency_count: int;
      object_count: int;
      copied_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | PackageExecutionPrepared of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      input_count: int;
      dependency_count: int;
      dependency_object_count: int;
      prepared_at: Time.Instant.t;
      duration: Time.Duration.t;
    }
  | PackageOcamlcWarnings of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      source: warning_source;
      messages: string list;
    }
  | BuildCompleted of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      status: [`Fresh | `Cached];
      duration: Time.Duration.t;
    }
  | BuildFailed of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      error: package_error;
    }
  | BuildSkipped of {
      session_id: Session_id.t;
      package: Package.t;
      target: Workspace_planner.target;
      build_target: Target.t;
      reason: string;
    }
  | ActionStarted of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      started_at: Time.Instant.t;
    }
  | ActionCommandStarted of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      started_at: Time.Instant.t;
      command: string;
    }
  | ActionCompleted of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      completed_at: Time.Instant.t;
      artifact: Artifact.t;
      status: [`Fresh | `Cached];
      duration: Time.Duration.t;
    }
  | ActionFailed of {
      session_id: Session_id.t;
      package: Package.t;
      build_target: Target.t;
      action: Action_node.t;
      failed_at: Time.Instant.t;
      error: string;
    }
  | CacheHit of {
      session_id: Session_id.t;
      package: Package.t;
      action: Action_node.t;
      hash: Crypto.hash;
    }
  | CacheMiss of {
      session_id: Session_id.t;
      package: Package.t;
      action: Action_node.t;
      hash: Crypto.hash;
    }
  | WorkspaceStarted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      package_count: int;
    }
  | WorkspaceCompleted of {
      session_id: Session_id.t;
      target: Workspace_planner.target;
      total_duration: Time.Duration.t;
      cached_count: int;
      built_count: int;
      failed_count: int;
    }

let target_to_json = fun target ->
  Data.Json.String (
    match target with
    | Workspace_planner.All -> "all"
    | Workspace_planner.Package pkg -> Package_name.to_string pkg
    | Workspace_planner.Packages pkgs ->
        "packages:" ^ String.concat "," (List.map pkgs ~fn:Package_name.to_string)
  )

let target_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String "all" -> Ok Workspace_planner.All
  | Data.Json.String target_str when String.starts_with ~prefix:"packages:" target_str ->
      let prefix_len = String.length "packages:" in
      let packages_str =
        String.sub target_str ~offset:prefix_len ~len:(String.length target_str - prefix_len)
      in
      let packages =
        if String.equal packages_str "" then
          Ok []
        else
          let rec loop = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok []
            | package_name :: rest -> (
                match Package_name.from_string package_name with
                | Error error -> Error (Data.Json.String (Package_name.error_message error))
                | Ok package_name -> (
                    match loop rest with
                    | Ok rest -> Ok (package_name :: rest)
                    | Error error -> Error error
                  )
              )
          in
          loop (String.split ~by:"," packages_str)
      in
      Result.map packages ~fn:(fun packages -> Workspace_planner.Packages packages)
  | Data.Json.String pkg ->
      Package_name.from_string pkg
      |> Result.map ~fn:(fun pkg -> Workspace_planner.Package pkg)
      |> Result.map_err ~fn:(fun error -> Data.Json.string (Package_name.error_message error))
  | _ -> Error (Data.Json.String "Invalid target")

let build_target_to_json = fun target -> Data.Json.String (Target.to_string target)

let build_target_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String target ->
      Target.from_string target
      |> Result.map_err ~fn:(fun error -> Data.Json.String (Target.error_message error))
  | _ -> Error (Data.Json.String "Invalid build target")

let build_target_from_fields = fun fields ->
  match Data.Json.get_field "build_target" (Data.Json.Object fields) with
  | Some build_target_json -> build_target_of_json build_target_json
  | None -> Ok Target.current

let action_to_json = fun (action: Action_node.t) ->
  let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
  Data.Json.Object [
    ("action_hash", Data.Json.String action_hash);
    ("action_node", Action_node.to_json action);
  ]

let planning_status_to_json = fun __tmp1 ->
  match __tmp1 with
  | `Planned -> Data.Json.String "planned"
  | `MissingDependencies -> Data.Json.String "missing_dependencies"
  | `FailedDependencies -> Data.Json.String "failed_dependencies"
  | `Failed -> Data.Json.String "failed"

let planning_status_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String "planned" -> Ok `Planned
  | Data.Json.String "missing_dependencies" -> Ok `MissingDependencies
  | Data.Json.String "failed_dependencies" -> Ok `FailedDependencies
  | Data.Json.String "failed" -> Ok `Failed
  | _ -> Error (Data.Json.String "Invalid planning status")

let warning_source_to_json = fun __tmp1 ->
  match __tmp1 with
  | `Fresh -> Data.Json.String "fresh"
  | `Cached -> Data.Json.String "cached"

let warning_source_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.String "fresh" -> Ok `Fresh
  | Data.Json.String "cached" -> Ok `Cached
  | _ -> Error (Data.Json.String "Invalid warning source")

let planning_breakdown_to_json breakdown =
  Data.Json.Object [
    ("dependency_count", Data.Json.Int breakdown.dependency_count);
    (
      "dependency_check_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.dependency_check_duration)
    );
    (
      "input_hash_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.input_hash_duration)
    );
    (
      "artifact_lookup_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.artifact_lookup_duration)
    );
    ("artifact_cache_hit", Data.Json.Bool breakdown.artifact_cache_hit);
    (
      "plan_bundle_lookup_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.plan_bundle_lookup_duration)
    );
    (
      "plan_bundle_decode_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.plan_bundle_decode_duration)
    );
    ("plan_bundle_cache_hit", Data.Json.Bool breakdown.plan_bundle_cache_hit);
    (
      "module_plan_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.module_plan_duration)
    );
  ]

let planning_breakdown_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Object fields -> (
      match (
        Data.Json.get_field "dependency_count" (Data.Json.Object fields),
        Data.Json.get_field "dependency_check_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "input_hash_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "artifact_lookup_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "artifact_cache_hit" (Data.Json.Object fields),
        Data.Json.get_field "plan_bundle_lookup_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "plan_bundle_decode_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "plan_bundle_cache_hit" (Data.Json.Object fields),
        Data.Json.get_field "module_plan_duration_ms" (Data.Json.Object fields)
      ) with
      | (
          Some (Data.Json.Int dependency_count),
          Some (Data.Json.Int dependency_check_duration_ms),
          Some (Data.Json.Int input_hash_duration_ms),
          Some (Data.Json.Int artifact_lookup_duration_ms),
          Some (Data.Json.Bool artifact_cache_hit),
          Some (Data.Json.Int plan_bundle_lookup_duration_ms),
          Some (Data.Json.Int plan_bundle_decode_duration_ms),
          Some (Data.Json.Bool plan_bundle_cache_hit),
          Some (Data.Json.Int module_plan_duration_ms)
        ) ->
          Ok {
            dependency_count;
            dependency_check_duration = Time.Duration.from_millis dependency_check_duration_ms;
            input_hash_duration = Time.Duration.from_millis input_hash_duration_ms;
            artifact_lookup_duration = Time.Duration.from_millis artifact_lookup_duration_ms;
            artifact_cache_hit;
            plan_bundle_lookup_duration = Time.Duration.from_millis plan_bundle_lookup_duration_ms;
            plan_bundle_decode_duration = Time.Duration.from_millis plan_bundle_decode_duration_ms;
            plan_bundle_cache_hit;
            module_plan_duration = Time.Duration.from_millis module_plan_duration_ms;
          }
      | _ -> Error (Data.Json.String "Invalid package planning breakdown")
    )
  | _ -> Error (Data.Json.String "Package planning breakdown must be an object")

let workspace_graph_breakdown_to_json breakdown =
  Data.Json.Object [
    ("build_node_realization_count", Data.Json.Int breakdown.build_node_realization_count);
    (
      "build_node_realization_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.build_node_realization_duration)
    );
    ("runtime_node_realization_count", Data.Json.Int breakdown.runtime_node_realization_count);
    (
      "runtime_node_realization_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.runtime_node_realization_duration)
    );
    ("dev_node_realization_count", Data.Json.Int breakdown.dev_node_realization_count);
    (
      "dev_node_realization_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.dev_node_realization_duration)
    );
    (
      "edge_wiring_duration_ms",
      Data.Json.Int (Time.Duration.to_millis breakdown.edge_wiring_duration)
    );
  ]

let workspace_graph_breakdown_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Object fields -> (
      match (
        Data.Json.get_field "build_node_realization_count" (Data.Json.Object fields),
        Data.Json.get_field "build_node_realization_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "runtime_node_realization_count" (Data.Json.Object fields),
        Data.Json.get_field "runtime_node_realization_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "dev_node_realization_count" (Data.Json.Object fields),
        Data.Json.get_field "dev_node_realization_duration_ms" (Data.Json.Object fields),
        Data.Json.get_field "edge_wiring_duration_ms" (Data.Json.Object fields)
      ) with
      | (
          Some (Data.Json.Int build_node_realization_count),
          Some (Data.Json.Int build_node_realization_duration_ms),
          Some (Data.Json.Int runtime_node_realization_count),
          Some (Data.Json.Int runtime_node_realization_duration_ms),
          Some (Data.Json.Int dev_node_realization_count),
          Some (Data.Json.Int dev_node_realization_duration_ms),
          Some (Data.Json.Int edge_wiring_duration_ms)
        ) ->
          Ok {
            build_node_realization_count;
            build_node_realization_duration = Time.Duration.from_millis
              build_node_realization_duration_ms;
            runtime_node_realization_count;
            runtime_node_realization_duration = Time.Duration.from_millis
              runtime_node_realization_duration_ms;
            dev_node_realization_count;
            dev_node_realization_duration = Time.Duration.from_millis
              dev_node_realization_duration_ms;
            edge_wiring_duration = Time.Duration.from_millis edge_wiring_duration_ms;
          }
      | _ -> Error (Data.Json.String "Invalid workspace graph breakdown")
    )
  | _ -> Error (Data.Json.String "Workspace graph breakdown must be an object")

let to_json: Telemetry.event -> Data.Json.t option = fun __tmp1 ->
  match __tmp1 with
  | PackageStarted {
      session_id;
      package;
      target;
      started_at = _;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PackageStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
      ])
  | WorkspacePlanStarted { session_id; target; workspace_package_count } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspacePlanStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("workspace_package_count", Data.Json.Int workspace_package_count);
      ])
  | WorkspacePlanCompleted {
      session_id;
      target;
      workspace_package_count;
      planned_package_count;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspacePlanCompleted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("workspace_package_count", Data.Json.Int workspace_package_count);
        ("planned_package_count", Data.Json.Int planned_package_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | WorkspaceManifestFilterCompleted {
      session_id;
      target;
      filtered_workspace_package_count;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspaceManifestFilterCompleted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("filtered_workspace_package_count", Data.Json.Int filtered_workspace_package_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | WorkspaceGraphCreated {
      session_id;
      target;
      node_count;
      breakdown;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspaceGraphCreated");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("node_count", Data.Json.Int node_count);
        ("breakdown", workspace_graph_breakdown_to_json breakdown);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | WorkspaceTargetGraphFiltered {
      session_id;
      target;
      node_count;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspaceTargetGraphFiltered");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("node_count", Data.Json.Int node_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | WorkspaceTopologicalSortCompleted {
      session_id;
      target;
      sorted_package_count;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspaceTopologicalSortCompleted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("sorted_package_count", Data.Json.Int sorted_package_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | PlanningWorkspaceStarted { session_id; target; package_count } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PlanningWorkspaceStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("package_count", Data.Json.Int package_count);
      ])
  | PlanningWorkspaceCompleted {
      session_id;
      target;
      duration;
      planned_count;
      missing_count;
      failed_count;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PlanningWorkspaceCompleted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
        ("planned_count", Data.Json.Int planned_count);
        ("missing_count", Data.Json.Int missing_count);
        ("failed_count", Data.Json.Int failed_count);
      ])
  | PackagePlanningResult {
      session_id;
      package;
      target;
      status;
      duration;
      reason;
    } ->
      Some (
        Data.Json.Object (
          [
            ("type", Data.Json.String "PackagePlanningResult");
            ("session_id", Data.Json.String (Session_id.to_string session_id));
            ("package", Package.to_json package);
            ("target", target_to_json target);
            ("status", planning_status_to_json status);
            ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
          ] @ match reason with
          | Some reason -> [ ("reason", Data.Json.String reason); ]
          | None -> []
        )
      )
  | PackagePlanningBreakdown {
      session_id;
      package;
      target;
      breakdown;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PackagePlanningBreakdown");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("breakdown", planning_breakdown_to_json breakdown);
      ])
  | CompilationStarted {
      session_id;
      package;
      target;
      build_target;
      action_count;
      started_at = _;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "CompilationStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("action_count", Data.Json.Int action_count);
      ])
  | SandboxCreated {
      session_id;
      package;
      target;
      build_target;
      path;
      created_at = _;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "SandboxCreated");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("path", Data.Json.String (Path.to_string path));
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | SandboxInputsCopied {
      session_id;
      package;
      target;
      build_target;
      input_count;
      copied_at = _;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "SandboxInputsCopied");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("input_count", Data.Json.Int input_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | SandboxDependenciesCopied {
      session_id;
      package;
      target;
      build_target;
      dependency_count;
      object_count;
      copied_at = _;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "SandboxDependenciesCopied");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("dependency_count", Data.Json.Int dependency_count);
        ("object_count", Data.Json.Int object_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | PackageExecutionPrepared {
      session_id;
      package;
      target;
      build_target;
      input_count;
      dependency_count;
      dependency_object_count;
      prepared_at = _;
      duration;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PackageExecutionPrepared");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("input_count", Data.Json.Int input_count);
        ("dependency_count", Data.Json.Int dependency_count);
        ("dependency_object_count", Data.Json.Int dependency_object_count);
        ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
      ])
  | PackageOcamlcWarnings {
      session_id;
      package;
      target;
      build_target;
      source;
      messages;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "PackageOcamlcWarnings");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("source", warning_source_to_json source);
        ("messages", Data.Json.Array (List.map messages ~fn:(fun msg -> Data.Json.String msg)));
      ])
  | BuildCompleted {
      session_id;
      package;
      target;
      build_target;
      status;
      duration;
    } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "BuildCompleted");
          ("session_id", Data.Json.String (Session_id.to_string session_id));
          ("package", Package.to_json package);
          ("target", target_to_json target);
          ("build_target", build_target_to_json build_target);
          ("status", Data.Json.String (
            match status with
            | `Fresh -> "fresh"
            | `Cached -> "cached"
          ));
          ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
        ]
      )
  | BuildFailed {
      session_id;
      package;
      target;
      build_target;
      error;
    } ->
      let error_json =
        match error with
        | PlanningFailed planning_err ->
            Data.Json.Object [
              ("type", Data.Json.String "planning_failed");
              ("error", Planning_error.to_json planning_err);
            ]
        | ExecutionFailed { message } ->
            Data.Json.Object [
              ("type", Data.Json.String "execution_failed");
              ("message", Data.Json.String message);
            ]
        | ActionExecutionFailed { message } ->
            Data.Json.Object [
              ("type", Data.Json.String "action_failed");
              ("message", Data.Json.String message);
            ]
        | ActionOutputsNotCreated { missing } ->
            Data.Json.Object [
              ("type", Data.Json.String "outputs_not_created");
              (
                "missing",
                Data.Json.Array (List.map missing ~fn:(fun p -> Data.Json.String (Path.to_string p)))
              );
            ]
        | ActionDependenciesFailed { failed } ->
            Data.Json.Object [
              ("type", Data.Json.String "dependencies_failed");
              ("failed_count", Data.Json.String (Int.to_string (List.length failed)));
            ]
      in
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildFailed");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("error", error_json);
      ])
  | BuildSkipped {
      session_id;
      package;
      target;
      build_target;
      reason;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildSkipped");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("build_target", build_target_to_json build_target);
        ("reason", Data.Json.String reason);
      ])
  | ActionStarted {
      session_id;
      package;
      build_target;
      action;
      started_at = _;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "ActionStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("build_target", build_target_to_json build_target);
        ("action", action_to_json action);
      ])
  | ActionCommandStarted {
      session_id;
      package;
      build_target;
      action;
      started_at = _;
      command;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "ActionCommandStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("build_target", build_target_to_json build_target);
        ("action", action_to_json action);
        ("command", Data.Json.String command);
      ])
  | ActionCompleted {
      session_id;
      package;
      build_target;
      action;
      completed_at = _;
      artifact;
      status;
      duration;
    } ->
      let artifact_files = Data.Json.Array (List.map
        artifact.files
        ~fn:(fun entry -> Data.Json.String (Path.to_string entry.Riot_store.Manifest.path)))
      in
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "ActionCompleted");
          ("session_id", Data.Json.String (Session_id.to_string session_id));
          ("package", Package.to_json package);
          ("build_target", build_target_to_json build_target);
          ("action", action_to_json action);
          ("artifact_files", artifact_files);
          ("status", Data.Json.String (
            match status with
            | `Fresh -> "fresh"
            | `Cached -> "cached"
          ));
          ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
        ]
      )
  | ActionFailed {
      session_id;
      package;
      build_target;
      action;
      failed_at = _;
      error;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "ActionFailed");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("build_target", build_target_to_json build_target);
        ("action", action_to_json action);
        ("error", Data.Json.String error);
      ])
  | CacheHit {
      session_id;
      package;
      action;
      hash;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "CacheHit");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("action", action_to_json action);
        ("hash", Data.Json.String (Crypto.Digest.hex hash));
      ])
  | CacheMiss {
      session_id;
      package;
      action;
      hash;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "CacheMiss");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("action", action_to_json action);
        ("hash", Data.Json.String (Crypto.Digest.hex hash));
      ])
  | WorkspaceStarted { session_id; target; package_count } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspaceStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("package_count", Data.Json.Int package_count);
      ])
  | WorkspaceCompleted {
      session_id;
      target;
      total_duration;
      cached_count;
      built_count;
      failed_count;
    } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "WorkspaceCompleted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("target", target_to_json target);
        ("total_duration_ms", Data.Json.Int (Time.Duration.to_millis total_duration));
        ("cached_count", Data.Json.Int cached_count);
        ("built_count", Data.Json.Int built_count);
        ("failed_count", Data.Json.Int failed_count);
      ])
  | _ -> None

let event_session_id: Telemetry.event -> Session_id.t option = fun __tmp1 ->
  match __tmp1 with
  | PackageStarted { session_id; _ }
  | WorkspacePlanStarted { session_id; _ }
  | WorkspacePlanCompleted { session_id; _ }
  | WorkspaceManifestFilterCompleted { session_id; _ }
  | WorkspaceGraphCreated { session_id; _ }
  | WorkspaceTargetGraphFiltered { session_id; _ }
  | WorkspaceTopologicalSortCompleted { session_id; _ }
  | PlanningWorkspaceStarted { session_id; _ }
  | PlanningWorkspaceCompleted { session_id; _ }
  | PackagePlanningResult { session_id; _ }
  | PackagePlanningBreakdown { session_id; _ }
  | CompilationStarted { session_id; _ }
  | SandboxCreated { session_id; _ }
  | SandboxInputsCopied { session_id; _ }
  | SandboxDependenciesCopied { session_id; _ }
  | PackageExecutionPrepared { session_id; _ }
  | PackageOcamlcWarnings { session_id; _ }
  | BuildCompleted { session_id; _ }
  | BuildFailed { session_id; _ }
  | BuildSkipped { session_id; _ }
  | ActionStarted { session_id; _ }
  | ActionCommandStarted { session_id; _ }
  | ActionCompleted { session_id; _ }
  | ActionFailed { session_id; _ }
  | CacheHit { session_id; _ }
  | CacheMiss { session_id; _ }
  | WorkspaceStarted { session_id; _ }
  | WorkspaceCompleted { session_id; _ } -> Some session_id
  | _ -> None

let event_timestamp: Telemetry.event -> (string * Time.Instant.t) option = fun __tmp1 ->
  match __tmp1 with
  | PackageStarted { started_at; _ }
  | CompilationStarted { started_at; _ }
  | ActionStarted { started_at; _ }
  | ActionCommandStarted { started_at; _ } -> Some ("started_at_us", started_at)
  | SandboxCreated { created_at; _ } -> Some ("created_at_us", created_at)
  | SandboxInputsCopied { copied_at; _ }
  | SandboxDependenciesCopied { copied_at; _ } -> Some ("copied_at_us", copied_at)
  | PackageExecutionPrepared { prepared_at; _ } -> Some ("prepared_at_us", prepared_at)
  | ActionCompleted { completed_at; _ } -> Some ("completed_at_us", completed_at)
  | ActionFailed { failed_at; _ } -> Some ("failed_at_us", failed_at)
  | _ -> None

let from_json: Data.Json.t -> (Telemetry.event, Data.Json.t) result = fun json ->
  let get_field fields ~name =
    List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name)
    |> Option.map ~fn:(fun (_, value) -> value)
  in
  let get_session_id fields =
    match get_field fields ~name:"session_id" with
    | Some (Data.Json.String sid) -> Session_id.from_string sid
    | _ -> Session_id.from_string "unknown"
  in
  let int_field_or_default fields ~name ~default =
    match get_field fields ~name with
    | Some (Data.Json.Int value) -> value
    | _ -> default
  in
  match json with
  | Data.Json.Object fields -> (
      match get_field fields ~name:"type" with
      | Some (Data.Json.String "PackageStarted") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target"
          ) with
          | (Some (Data.Json.String session_id_str), Some package_json, Some target_json) -> (
              match Package.from_json package_json with
              | Ok package ->
                  let session_id = Session_id.from_string session_id_str in
                  (
                    match target_of_json target_json with
                    | Ok target ->
                        Ok (
                          PackageStarted {
                            session_id;
                            package;
                            target;
                            started_at = Time.Instant.now ();
                          }
                        )
                    | Error e -> Error e
                  )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid PackageStarted event")
        )
      | Some (Data.Json.String "WorkspacePlanStarted") -> (
          match (get_field fields ~name:"target", get_field fields ~name:"workspace_package_count") with
          | (Some (Data.Json.String target_str), Some (Data.Json.Int workspace_package_count)) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (WorkspacePlanStarted {
                    session_id = get_session_id fields;
                    target;
                    workspace_package_count;
                  })
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspacePlanStarted event")
        )
      | Some (Data.Json.String "WorkspacePlanCompleted") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"workspace_package_count",
            get_field fields ~name:"planned_package_count",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int workspace_package_count),
              Some (Data.Json.Int planned_package_count),
              Some (Data.Json.Int duration_ms)
            ) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (
                    WorkspacePlanCompleted {
                      session_id = get_session_id fields;
                      target;
                      workspace_package_count;
                      planned_package_count;
                      duration = Time.Duration.from_millis duration_ms;
                    }
                  )
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspacePlanCompleted event")
        )
      | Some (Data.Json.String "WorkspaceManifestFilterCompleted") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"filtered_workspace_package_count",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int filtered_workspace_package_count),
              Some (Data.Json.Int duration_ms)
            ) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (
                    WorkspaceManifestFilterCompleted {
                      session_id = get_session_id fields;
                      target;
                      filtered_workspace_package_count;
                      duration = Time.Duration.from_millis duration_ms;
                    }
                  )
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceManifestFilterCompleted event")
        )
      | Some (Data.Json.String "WorkspaceGraphCreated") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"node_count",
            get_field fields ~name:"breakdown",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int node_count),
              Some breakdown_json,
              Some (Data.Json.Int duration_ms)
            ) -> (
              match (
                target_of_json (Data.Json.String target_str),
                workspace_graph_breakdown_of_json breakdown_json
              ) with
              | (Ok target, Ok breakdown) ->
                  Ok (
                    WorkspaceGraphCreated {
                      session_id = get_session_id fields;
                      target;
                      node_count;
                      breakdown;
                      duration = Time.Duration.from_millis duration_ms;
                    }
                  )
              | (Error e, _)
              | (_, Error e) -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceGraphCreated event")
        )
      | Some (Data.Json.String "WorkspaceTargetGraphFiltered") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"node_count",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int node_count),
              Some (Data.Json.Int duration_ms)
            ) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (
                    WorkspaceTargetGraphFiltered {
                      session_id = get_session_id fields;
                      target;
                      node_count;
                      duration = Time.Duration.from_millis duration_ms;
                    }
                  )
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceTargetGraphFiltered event")
        )
      | Some (Data.Json.String "WorkspaceTopologicalSortCompleted") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"sorted_package_count",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int sorted_package_count),
              Some (Data.Json.Int duration_ms)
            ) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (
                    WorkspaceTopologicalSortCompleted {
                      session_id = get_session_id fields;
                      target;
                      sorted_package_count;
                      duration = Time.Duration.from_millis duration_ms;
                    }
                  )
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceTopologicalSortCompleted event")
        )
      | Some (Data.Json.String "PlanningWorkspaceStarted") -> (
          match (get_field fields ~name:"target", get_field fields ~name:"package_count") with
          | (Some (Data.Json.String target_str), Some (Data.Json.Int package_count)) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (PlanningWorkspaceStarted {
                    session_id = get_session_id fields;
                    target;
                    package_count;
                  })
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid PlanningWorkspaceStarted event")
        )
      | Some (Data.Json.String "PlanningWorkspaceCompleted") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"duration_ms",
            get_field fields ~name:"planned_count",
            get_field fields ~name:"missing_count",
            get_field fields ~name:"failed_count"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int duration_ms),
              Some (Data.Json.Int planned_count),
              Some (Data.Json.Int missing_count),
              Some (Data.Json.Int failed_count)
            ) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (
                    PlanningWorkspaceCompleted {
                      session_id = get_session_id fields;
                      target;
                      duration = Time.Duration.from_millis duration_ms;
                      planned_count;
                      missing_count;
                      failed_count;
                    }
                  )
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid PlanningWorkspaceCompleted event")
        )
      | Some (Data.Json.String "PackagePlanningResult") -> (
          match (
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"status",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some package_json,
              Some (Data.Json.String target_str),
              Some status_json,
              Some (Data.Json.Int duration_ms)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match (
                    target_of_json (Data.Json.String target_str),
                    planning_status_of_json status_json
                  ) with
                  | (Ok target, Ok status) ->
                      Ok (
                        PackagePlanningResult {
                          session_id = get_session_id fields;
                          package;
                          target;
                          status;
                          duration = Time.Duration.from_millis duration_ms;
                          reason =
                            (
                              match get_field fields ~name:"reason" with
                              | Some (Data.Json.String reason) -> Some reason
                              | _ -> None
                            );
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid PackagePlanningResult event")
        )
      | Some (Data.Json.String "PackagePlanningBreakdown") -> (
          match (
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"breakdown"
          ) with
          | (Some package_json, Some (Data.Json.String target_str), Some breakdown_json) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match (
                    target_of_json (Data.Json.String target_str),
                    planning_breakdown_of_json breakdown_json
                  ) with
                  | (Ok target, Ok breakdown) ->
                      Ok (
                        PackagePlanningBreakdown {
                          session_id = get_session_id fields;
                          package;
                          target;
                          breakdown;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid PackagePlanningBreakdown event")
        )
      | Some (Data.Json.String "CompilationStarted") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target"
          ) with
          | (Some (Data.Json.String session_id_str), Some package_json, Some target_json) -> (
              match Package.from_json package_json with
              | Ok package ->
                  let session_id = Session_id.from_string session_id_str in
                  (
                    match (target_of_json target_json, build_target_from_fields fields) with
                    | (Ok target, Ok build_target) ->
                        Ok (
                          CompilationStarted {
                            session_id;
                            package;
                            target;
                            build_target;
                            action_count = int_field_or_default
                              fields
                              ~name:"action_count"
                              ~default:0;
                            started_at = Time.Instant.now ();
                          }
                        )
                    | (Error e, _) -> Error e
                    | (_, Error e) -> Error e
                  )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid CompilationStarted event")
        )
      | Some (Data.Json.String "SandboxCreated") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"path",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String session_id_str),
              Some package_json,
              Some target_json,
              Some (Data.Json.String path),
              Some (Data.Json.Int duration_ms)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  let session_id = Session_id.from_string session_id_str in
                  match (target_of_json target_json, build_target_from_fields fields) with
                  | (Ok target, Ok build_target) ->
                      Ok (
                        SandboxCreated {
                          session_id;
                          package;
                          target;
                          build_target;
                          path = Path.v path;
                          created_at = Time.Instant.now ();
                          duration = Time.Duration.from_millis duration_ms;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid SandboxCreated event")
        )
      | Some (Data.Json.String "SandboxInputsCopied") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String session_id_str),
              Some package_json,
              Some target_json,
              Some (Data.Json.Int duration_ms)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  let session_id = Session_id.from_string session_id_str in
                  match (target_of_json target_json, build_target_from_fields fields) with
                  | (Ok target, Ok build_target) ->
                      Ok (
                        SandboxInputsCopied {
                          session_id;
                          package;
                          target;
                          build_target;
                          input_count = int_field_or_default
                            fields
                            ~name:"input_count"
                            ~default:0;
                          copied_at = Time.Instant.now ();
                          duration = Time.Duration.from_millis duration_ms;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid SandboxInputsCopied event")
        )
      | Some (Data.Json.String "SandboxDependenciesCopied") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String session_id_str),
              Some package_json,
              Some target_json,
              Some (Data.Json.Int duration_ms)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  let session_id = Session_id.from_string session_id_str in
                  match (target_of_json target_json, build_target_from_fields fields) with
                  | (Ok target, Ok build_target) ->
                      Ok (
                        SandboxDependenciesCopied {
                          session_id;
                          package;
                          target;
                          build_target;
                          dependency_count = int_field_or_default
                            fields
                            ~name:"dependency_count"
                            ~default:0;
                          object_count = int_field_or_default
                            fields
                            ~name:"object_count"
                            ~default:0;
                          copied_at = Time.Instant.now ();
                          duration = Time.Duration.from_millis duration_ms;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid SandboxDependenciesCopied event")
        )
      | Some (Data.Json.String "PackageExecutionPrepared") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some (Data.Json.String session_id_str),
              Some package_json,
              Some target_json,
              Some (Data.Json.Int duration_ms)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  let session_id = Session_id.from_string session_id_str in
                  match (target_of_json target_json, build_target_from_fields fields) with
                  | (Ok target, Ok build_target) ->
                      Ok (
                        PackageExecutionPrepared {
                          session_id;
                          package;
                          target;
                          build_target;
                          input_count = int_field_or_default
                            fields
                            ~name:"input_count"
                            ~default:0;
                          dependency_count = int_field_or_default
                            fields
                            ~name:"dependency_count"
                            ~default:0;
                          dependency_object_count = int_field_or_default
                            fields
                            ~name:"dependency_object_count"
                            ~default:0;
                          prepared_at = Time.Instant.now ();
                          duration = Time.Duration.from_millis duration_ms;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid PackageExecutionPrepared event")
        )
      | Some (Data.Json.String "PackageOcamlcWarnings") -> (
          match (
            get_field fields ~name:"session_id",
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"source",
            get_field fields ~name:"messages"
          ) with
          | (
              Some (Data.Json.String session_id_str),
              Some package_json,
              Some target_json,
              Some source_json,
              Some (Data.Json.Array messages_json)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match (
                    target_of_json target_json,
                    build_target_from_fields fields,
                    warning_source_of_json source_json
                  ) with
                  | (Ok target, Ok build_target, Ok source) ->
                      let rec collect_messages acc = fun __tmp1 ->
                        match __tmp1 with
                        | [] -> Ok (List.reverse acc)
                        | (Data.Json.String msg) :: rest -> collect_messages (msg :: acc) rest
                        | _ -> Error (Data.Json.String "Invalid PackageOcamlcWarnings messages")
                      in
                      (
                        match collect_messages [] messages_json with
                        | Ok messages ->
                            let session_id = Session_id.from_string session_id_str in
                            Ok (
                              PackageOcamlcWarnings {
                                session_id;
                                package;
                                target;
                                build_target;
                                source;
                                messages;
                              }
                            )
                        | Error e -> Error e
                      )
                  | (Error e, _, _) -> Error e
                  | (_, Error e, _) -> Error e
                  | (_, _, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid PackageOcamlcWarnings event")
        )
      | Some (Data.Json.String "BuildCompleted") -> (
          match (
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"status",
            get_field fields ~name:"duration_ms"
          ) with
          | (
              Some package_json,
              Some target_json,
              Some (Data.Json.String status_str),
              Some (Data.Json.Int duration_ms)
            ) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match (target_of_json target_json, build_target_from_fields fields) with
                  | (Ok target, Ok build_target) ->
                      let status =
                        match status_str with
                        | "cached" -> `Cached
                        | _ -> `Fresh
                      in
                      let duration = Time.Duration.from_millis duration_ms in
                      Ok (
                        BuildCompleted {
                          session_id = get_session_id fields;
                          package;
                          target;
                          build_target;
                          status;
                          duration;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildCompleted event")
        )
      | Some (Data.Json.String "BuildFailed") -> (
          match (
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"error"
          ) with
          | (Some package_json, Some target_json, Some error_json) -> (
              match Package.from_json package_json with
              | Ok package ->
                  (* Deserialize structured error *)
                  let error_result =
                    match error_json with
                    | Data.Json.Object error_fields -> (
                        match get_field error_fields ~name:"type" with
                        | Some (Data.Json.String "planning_failed") -> (
                            (* For planning errors, try to deserialize from the nested error field *)
                            match get_field error_fields ~name:"error" with
                            | Some (Data.Json.Object planning_fields) -> (
                                match get_field planning_fields ~name:"type" with
                                | Some (Data.Json.String "cyclic_dependency") -> (
                                    match get_field planning_fields ~name:"cycle" with
                                    | Some (Data.Json.Array arr) ->
                                        let cycle =
                                          List.filter_map
                                            arr
                                            ~fn:(fun __tmp1 ->
                                              match __tmp1 with
                                              | Data.Json.String s -> Some s
                                              | _ -> None)
                                        in
                                        Ok (PlanningFailed (Planning_error.CyclicDependency {
                                          cycle;
                                        }))
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: cyclic dependency";
                                        })
                                  )
                                | Some (Data.Json.String "scan_failed") -> (
                                    match (
                                      get_field planning_fields ~name:"path",
                                      get_field planning_fields ~name:"reason"
                                    ) with
                                    | (
                                        Some (Data.Json.String path),
                                        Some (Data.Json.String reason)
                                      ) ->
                                        Ok (PlanningFailed (Planning_error.ScanFailed {
                                          path = Path.v path;
                                          reason;
                                        }))
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: scan failed";
                                        })
                                  )
                                | Some (Data.Json.String "dependency_analysis_failed") -> (
                                    match get_field planning_fields ~name:"reason" with
                                    | Some (Data.Json.String reason) ->
                                        Ok (PlanningFailed (Planning_error.DependencyAnalysisFailed {
                                          reason;
                                        }))
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: dependency analysis failed";
                                        })
                                  )
                                | Some (Data.Json.String "graph_build_failed") -> (
                                    match get_field planning_fields ~name:"reason" with
                                    | Some (Data.Json.String reason) ->
                                        Ok (PlanningFailed (Planning_error.GraphBuildFailed {
                                          reason;
                                        }))
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: graph build failed";
                                        })
                                  )
                                | Some (
                                  Data.Json.String "source_depends_on_undeclared_package_module"
                                ) ->
                                    (
                                        match (
                                          get_field planning_fields ~name:"package_name",
                                          get_field planning_fields ~name:"source",
                                          get_field planning_fields ~name:"requested_module",
                                          get_field planning_fields ~name:"allowed_modules"
                                        ) with
                                        | (
                                            Some (Data.Json.String package_name),
                                            Some (Data.Json.String source),
                                            Some (Data.Json.String requested_module),
                                            Some (Data.Json.Array allowed_modules)
                                          ) ->
                                            let allowed_modules =
                                              List.filter_map
                                                allowed_modules
                                                ~fn:(fun __tmp1 ->
                                                  match __tmp1 with
                                                  | Data.Json.String allowed_module ->
                                                      Some allowed_module
                                                  | _ -> None)
                                            in
                                            let suggested_modules =
                                              match get_field
                                                planning_fields
                                                ~name:"suggested_modules" with
                                              | Some (Data.Json.Array suggested_modules) ->
                                                  List.filter_map
                                                    suggested_modules
                                                    ~fn:(fun __tmp1 ->
                                                      match __tmp1 with
                                                      | Data.Json.String suggested_module ->
                                                          Some suggested_module
                                                      | _ -> None)
                                              | _ -> []
                                            in
                                            Ok (
                                              PlanningFailed (
                                                Planning_error.SourceDependsOnUndeclaredPackageModule {
                                                  package_name;
                                                  source = Path.v source;
                                                  requested_module;
                                                  allowed_modules;
                                                  suggested_modules;
                                                }
                                              )
                                            )
                                        | _ ->
                                            Ok (ExecutionFailed {
                                              message = "Planning failed: source depends on undeclared package module";
                                            })
                                      )
                                | Some (
                                  Data.Json.String "target_depends_on_internal_library_module"
                                ) ->
                                    (
                                        match (
                                          get_field planning_fields ~name:"target_name",
                                          get_field planning_fields ~name:"source",
                                          get_field planning_fields ~name:"requested_module",
                                          get_field planning_fields ~name:"internal_module",
                                          get_field planning_fields ~name:"public_module"
                                        ) with
                                        | (
                                            Some (Data.Json.String target_name),
                                            Some (Data.Json.String source),
                                            Some (Data.Json.String requested_module),
                                            Some (Data.Json.String internal_module),
                                            Some (Data.Json.String public_module)
                                          ) ->
                                            Ok (
                                              PlanningFailed (
                                                Planning_error.TargetDependsOnInternalLibraryModule {
                                                  target_name;
                                                  source = Path.v source;
                                                  requested_module;
                                                  internal_module;
                                                  public_module;
                                                }
                                              )
                                            )
                                        | _ ->
                                            Ok (ExecutionFailed {
                                              message = "Planning failed: target depends on internal library module";
                                            })
                                      )
                                | Some (
                                  Data.Json.String "target_depends_on_namespaced_internal_library_module"
                                ) ->
                                    (
                                        match (
                                          get_field planning_fields ~name:"target_name",
                                          get_field planning_fields ~name:"source",
                                          get_field planning_fields ~name:"requested_module",
                                          get_field planning_fields ~name:"internal_module",
                                          get_field planning_fields ~name:"public_module"
                                        ) with
                                        | (
                                            Some (Data.Json.String target_name),
                                            Some (Data.Json.String source),
                                            Some (Data.Json.String requested_module),
                                            Some (Data.Json.String internal_module),
                                            Some (Data.Json.String public_module)
                                          ) ->
                                            Ok (
                                              PlanningFailed (
                                                Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
                                                  target_name;
                                                  source = Path.v source;
                                                  requested_module;
                                                  internal_module;
                                                  public_module;
                                                }
                                              )
                                            )
                                        | _ ->
                                            Ok (ExecutionFailed {
                                              message = "Planning failed: target depends on namespaced internal library module";
                                            })
                                      )
                                | Some (Data.Json.String "target_depends_on_other_target_root") -> (
                                    match (
                                      get_field planning_fields ~name:"target_name",
                                      get_field planning_fields ~name:"source",
                                      get_field planning_fields ~name:"requested_module",
                                      get_field planning_fields ~name:"other_target_name",
                                      get_field planning_fields ~name:"other_target_module",
                                      get_field planning_fields ~name:"public_module"
                                    ) with
                                    | (
                                        Some (Data.Json.String target_name),
                                        Some (Data.Json.String source),
                                        Some (Data.Json.String requested_module),
                                        Some (Data.Json.String other_target_name),
                                        Some (Data.Json.String other_target_module),
                                        Some (Data.Json.String public_module)
                                      ) ->
                                        Ok (
                                          PlanningFailed (
                                            Planning_error.TargetDependsOnOtherTargetRoot {
                                              target_name;
                                              source = Path.v source;
                                              requested_module;
                                              other_target_name;
                                              other_target_module;
                                              public_module;
                                            }
                                          )
                                        )
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: target depends on other target root";
                                        })
                                  )
                                | Some (Data.Json.String "invalid_executable_main") -> (
                                    let executable_main_error_of_json = fun __tmp1 ->
                                      match __tmp1 with
                                      | Data.Json.Object fields -> (
                                          match get_field fields ~name:"type" with
                                          | Some (Data.Json.String "missing_main") ->
                                              Some Planning_error.MissingMain
                                          | Some (Data.Json.String "multiple_main_definitions") -> (
                                              match get_field fields ~name:"count" with
                                              | Some (Data.Json.Int count) ->
                                                  Some (Planning_error.MultipleMainDefinitions {
                                                    count;
                                                  })
                                              | _ -> None
                                            )
                                          | Some (Data.Json.String "invalid_main_parameters") -> (
                                              match get_field fields ~name:"parameters" with
                                              | Some (Data.Json.Array parameters) ->
                                                  let parameters =
                                                    List.filter_map
                                                      parameters
                                                      ~fn:(fun __tmp1 ->
                                                        match __tmp1 with
                                                        | Data.Json.String parameter ->
                                                            Some parameter
                                                        | _ -> None)
                                                  in
                                                  Some (Planning_error.InvalidMainParameters {
                                                    parameters;
                                                  })
                                              | _ -> None
                                            )
                                          | _ -> None
                                        )
                                      | _ -> None
                                    in
                                    match (
                                      get_field planning_fields ~name:"package_name",
                                      get_field planning_fields ~name:"target_name",
                                      get_field planning_fields ~name:"source",
                                      get_field planning_fields ~name:"file",
                                      get_field planning_fields ~name:"error"
                                    ) with
                                    | (
                                        package_name_json,
                                        Some (Data.Json.String target_name),
                                        Some (Data.Json.String source),
                                        file_json,
                                        Some error_json
                                      ) -> (
                                        match executable_main_error_of_json error_json with
                                        | Some error ->
                                            let package_name =
                                              match package_name_json with
                                              | Some (Data.Json.String package_name) -> package_name
                                              | _ -> "<unknown>"
                                            in
                                            let file =
                                              match file_json with
                                              | Some (Data.Json.String file) -> Path.v file
                                              | _ -> Path.v source
                                            in
                                            Ok (
                                              PlanningFailed (
                                                Planning_error.InvalidExecutableMain {
                                                  package_name;
                                                  target_name;
                                                  source = Path.v source;
                                                  file;
                                                  error;
                                                }
                                              )
                                            )
                                        | None ->
                                            Ok (ExecutionFailed {
                                              message = "Planning failed: invalid executable main";
                                            })
                                      )
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: invalid executable main";
                                        })
                                  )
                                | Some (Data.Json.String "exception") -> (
                                    match get_field planning_fields ~name:"message" with
                                    | Some (Data.Json.String msg) ->
                                        Ok (PlanningFailed (Planning_error.Exception {
                                          exn = Failure msg;
                                        }))
                                    | _ ->
                                        Ok (ExecutionFailed {
                                          message = "Planning failed: exception";
                                        })
                                  )
                                | _ ->
                                    Ok (ExecutionFailed {
                                      message = "Planning failed: unknown planning error";
                                    })
                              )
                            | _ ->
                                Ok (ExecutionFailed {
                                  message = "Planning failed: missing error details";
                                })
                          )
                        | Some (Data.Json.String "execution_failed") -> (
                            match get_field error_fields ~name:"message" with
                            | Some (Data.Json.String msg) -> Ok (ExecutionFailed { message = msg })
                            | _ ->
                                Ok (ExecutionFailed {
                                  message = "Execution failed: missing message";
                                })
                          )
                        | Some (Data.Json.String "action_failed") -> (
                            match get_field error_fields ~name:"message" with
                            | Some (Data.Json.String msg) ->
                                Ok (ActionExecutionFailed { message = msg })
                            | _ ->
                                Ok (ExecutionFailed { message = "Action failed: missing message" })
                          )
                        | Some (Data.Json.String "outputs_not_created") -> (
                            match get_field error_fields ~name:"missing" with
                            | Some (Data.Json.Array arr) ->
                                let missing =
                                  List.filter_map
                                    arr
                                    ~fn:(fun __tmp1 ->
                                      match __tmp1 with
                                      | Data.Json.String s -> Some (Path.v s)
                                      | _ -> None)
                                in
                                Ok (ActionOutputsNotCreated { missing })
                            | _ ->
                                Ok (ExecutionFailed {
                                  message = "Outputs not created: missing list";
                                })
                          )
                        | Some (Data.Json.String "dependencies_failed") ->
                            Ok (ActionDependenciesFailed { failed = [] })
                        | _ -> Ok (ExecutionFailed { message = "Unknown error type" })
                      )
                    | _ -> Ok (ExecutionFailed { message = "Invalid error format" })
                  in
                  (
                    match (
                      target_of_json target_json,
                      build_target_from_fields fields,
                      error_result
                    ) with
                    | (Ok target, Ok build_target, Ok error) ->
                        Ok (
                          BuildFailed {
                            session_id = get_session_id fields;
                            package;
                            target;
                            build_target;
                            error;
                          }
                        )
                    | (Error e, _, _) -> Error e
                    | (_, Error e, _) -> Error e
                    | (_, _, Error e) -> Error e
                  )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildFailed event")
        )
      | Some (Data.Json.String "BuildSkipped") -> (
          match (
            get_field fields ~name:"package",
            get_field fields ~name:"target",
            get_field fields ~name:"reason"
          ) with
          | (Some package_json, Some target_json, Some (Data.Json.String reason)) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match (target_of_json target_json, build_target_from_fields fields) with
                  | (Ok target, Ok build_target) ->
                      Ok (
                        BuildSkipped {
                          session_id = get_session_id fields;
                          package;
                          target;
                          build_target;
                          reason;
                        }
                      )
                  | (Error e, _) -> Error e
                  | (_, Error e) -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildSkipped event")
        )
      | Some (Data.Json.String "WorkspaceStarted") -> (
          match (get_field fields ~name:"target", get_field fields ~name:"package_count") with
          | (Some (Data.Json.String target_str), Some (Data.Json.Int package_count)) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  Ok (WorkspaceStarted {
                    session_id = get_session_id fields;
                    target;
                    package_count;
                  })
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceStarted event")
        )
      | Some (Data.Json.String "WorkspaceCompleted") -> (
          match (
            get_field fields ~name:"target",
            get_field fields ~name:"total_duration_ms",
            get_field fields ~name:"cached_count",
            get_field fields ~name:"built_count",
            get_field fields ~name:"failed_count"
          ) with
          | (
              Some (Data.Json.String target_str),
              Some (Data.Json.Int total_duration_ms),
              Some (Data.Json.Int cached_count),
              Some (Data.Json.Int built_count),
              Some (Data.Json.Int failed_count)
            ) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  let total_duration = Time.Duration.from_millis total_duration_ms in
                  Ok (
                    WorkspaceCompleted {
                      session_id = get_session_id fields;
                      target;
                      total_duration;
                      cached_count;
                      built_count;
                      failed_count;
                    }
                  )
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceCompleted event")
        )
      | Some (Data.Json.String "ActionStarted")
      | Some (Data.Json.String "ActionCommandStarted")
      | Some (Data.Json.String "ActionCompleted")
      | Some (Data.Json.String "ActionFailed")
      | Some (Data.Json.String "CacheHit")
      | Some (Data.Json.String "CacheMiss") ->
          Error (Data.Json.String "Action events require Action_node.t (not serializable)")
      | Some (Data.Json.String typ) ->
          Error (Data.Json.String ("Unknown telemetry event type: " ^ typ))
      | None -> Error (Data.Json.String "Missing 'type' field in telemetry event")
      | _ -> Error (Data.Json.String "Invalid 'type' field in telemetry event")
    )
  | _ -> Error (Data.Json.String "Telemetry event must be a JSON object")
