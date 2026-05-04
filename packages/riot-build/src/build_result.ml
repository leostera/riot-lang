open Std

type package_scope =
  | Unknown
  | Build
  | Runtime
  | Dev

type failure = {
  package_name: Riot_model.Package_name.t;
  package_key: Riot_model.Package.key;
  reason: failure_reason;
  message: string;
  ocamlc_warnings: string list;
  duration_ms: int;
}

and failure_reason =
  | PackagePlanningFailed of Riot_planner.Planning_error.t
  | PackageExecutionFailed of { message: string }
  | PackageActionFailed of { message: string }
  | PackageActionOutputsNotCreated of {
      missing: Std.Path.t list;
    }
  | PackageActionDependenciesFailed of {
      failed: Std.Graph.SimpleGraph.Node_id.t list;
    }
  | PackageSkipped of { reason: string }
  | UnknownFailure

type package_status =
  | Built of Riot_store.Artifact.t
  | Cached of Riot_store.Artifact.t
  | Skipped of string
  | Failed of string

type package_result = {
  package_name: Riot_model.Package_name.t;
  scope: package_scope;
  status: package_status;
  artifacts: Riot_store.Artifact.t list;
}

type t = {
  packages: package_result list;
}

let package_status_of_build_status = fun __tmp1 ->
  match __tmp1 with
  | Package_builder.Built artifact -> Built artifact
  | Package_builder.Cached artifact -> Cached artifact
  | Package_builder.Skipped { reason } -> Skipped reason
  | Package_builder.Failed error -> Failed (Package_builder.package_error_to_string error)

let artifact_of_status = fun __tmp1 ->
  match __tmp1 with
  | Built artifact
  | Cached artifact -> Some artifact
  | Skipped _
  | Failed _ -> None

let scope_of_package_key = fun key ->
  let value = Riot_model.Package.key_to_string key in
  if String.ends_with ~suffix:":dev" value then
    Dev
  else if String.ends_with ~suffix:":runtime" value then
    Runtime
  else if String.ends_with ~suffix:":build" value then
    Build
  else
    Unknown

let scope_priority = fun __tmp1 ->
  match __tmp1 with
  | Unknown -> 0
  | Build -> 1
  | Runtime -> 2
  | Dev -> 3

let status_priority = fun __tmp1 ->
  match __tmp1 with
  | Failed _ -> 0
  | Skipped _ -> 1
  | Cached _ -> 2
  | Built _ -> 3

let should_prefer = fun current incoming ->
  let scope_comparison =
    Int.compare (scope_priority incoming.scope) (scope_priority current.scope)
  in
  if scope_comparison = Order.GT then
    true
  else if scope_comparison = Order.LT then
    false
  else
    Int.compare (status_priority incoming.status) (status_priority current.status) = Order.GT

let merge_artifacts = fun current incoming ->
  List.fold_left
    incoming
    ~init:current
    ~fn:(fun acc (artifact: Riot_store.Artifact.t) ->
      if
        List.any
          acc
          ~fn:(fun existing ->
            Crypto.Hash.equal
              existing.Riot_store.Artifact.input_hash
              artifact.Riot_store.Artifact.input_hash)
      then
        acc
      else
        acc @ [ artifact ])

let merge_package_result = fun current incoming ->
  let preferred =
    if should_prefer current incoming then
      incoming
    else
      current
  in
  {
    package_name = current.package_name;
    scope = preferred.scope;
    status = preferred.status;
    artifacts = merge_artifacts current.artifacts incoming.artifacts;
  }

let rec upsert_package_result = fun packages incoming ->
  match packages with
  | [] -> [ incoming ]
  | current :: rest ->
      if Riot_model.Package_name.equal current.package_name incoming.package_name then
        merge_package_result current incoming :: rest
      else
        current :: upsert_package_result rest incoming

let package_result_of_build_result = fun (result: Package_builder.build_result) ->
  let status = package_status_of_build_status result.status in
  {
    package_name = result.package.name;
    scope = scope_of_package_key result.package_key;
    status;
    artifacts = Option.to_list (artifact_of_status status);
  }

let from_build_results = fun results -> {
  packages = List.fold_left
    results
    ~init:[]
    ~fn:(fun acc result -> upsert_package_result acc (package_result_of_build_result result));
}

let packages = fun t -> t.packages

let find_package = fun t name ->
  List.find
    t.packages
    ~fn:(fun pkg -> Riot_model.Package_name.equal pkg.package_name name)

let package_name = fun t -> t.package_name

let package_status = fun t -> t.status

let package_artifact = fun t -> artifact_of_status t.status

let rec find_export_in_artifacts = fun artifacts export_name ->
  match artifacts with
  | [] -> None
  | (artifact: Riot_store.Artifact.t) :: rest -> (
      match List.find
        artifact.exports
        ~fn:(fun (entry: Riot_store.Manifest.export_entry) -> String.equal entry.name export_name) with
      | Some entry -> Some entry
      | None -> find_export_in_artifacts rest export_name
    )

let find_export = fun t export_name -> find_export_in_artifacts t.artifacts export_name

let failure_reason_of_package_error = fun __tmp1 ->
  match __tmp1 with
  | Package_builder.PlanningFailed planning_error -> PackagePlanningFailed planning_error
  | Package_builder.ExecutionFailed { message } -> PackageExecutionFailed { message }
  | Package_builder.ActionExecutionFailed { message } -> PackageActionFailed { message }
  | Package_builder.ActionOutputsNotCreated { missing } ->
      PackageActionOutputsNotCreated { missing }
  | Package_builder.ActionDependenciesFailed { failed } ->
      PackageActionDependenciesFailed { failed }

let failure_reason_message = fun __tmp1 ->
  match __tmp1 with
  | PackagePlanningFailed planning_error ->
      "Planning failed: " ^ Riot_planner.Planning_error.to_string planning_error
  | PackageExecutionFailed { message } -> "Execution failed: " ^ message
  | PackageActionFailed { message } -> "Action failed: " ^ message
  | PackageActionOutputsNotCreated { missing } ->
      "Outputs not created: " ^ String.concat ", " (List.map missing ~fn:Path.to_string)
  | PackageActionDependenciesFailed { failed } ->
      "Dependencies failed: " ^ Int.to_string (List.length failed) ^ " actions"
  | PackageSkipped { reason } -> "Skipped: " ^ reason
  | UnknownFailure -> "Build failed"

let failure_reason_to_json = fun __tmp1 ->
  match __tmp1 with
  | PackagePlanningFailed planning_error ->
      Data.Json.Object [
        ("type", Data.Json.String "planning_failed");
        ("error", Riot_planner.Planning_error.to_json planning_error);
      ]
  | PackageExecutionFailed { message } ->
      Data.Json.Object [
        ("type", Data.Json.String "execution_failed");
        ("message", Data.Json.String message);
      ]
  | PackageActionFailed { message } ->
      Data.Json.Object [
        ("type", Data.Json.String "action_failed");
        ("message", Data.Json.String message);
      ]
  | PackageActionOutputsNotCreated { missing } ->
      Data.Json.Object [
        ("type", Data.Json.String "outputs_not_created");
        (
          "missing",
          Data.Json.Array (List.map missing ~fn:(fun path -> Data.Json.String (Path.to_string path)))
        );
      ]
  | PackageActionDependenciesFailed { failed } ->
      Data.Json.Object [
        ("type", Data.Json.String "dependencies_failed");
        ("failed_count", Data.Json.Int (List.length failed));
      ]
  | PackageSkipped { reason } ->
      Data.Json.Object [
        ("type", Data.Json.String "skipped");
        ("reason", Data.Json.String reason);
      ]
  | UnknownFailure -> Data.Json.Object [ ("type", Data.Json.String "unknown"); ]

let failure_of_build_result = fun (result: Package_builder.build_result) ->
  let reason =
    match result.status with
    | Package_builder.Failed error -> failure_reason_of_package_error error
    | Package_builder.Skipped { reason } -> PackageSkipped { reason }
    | Package_builder.Built _
    | Package_builder.Cached _ -> UnknownFailure
  in
  let message = failure_reason_message reason in
  {
    package_name = result.package.name;
    package_key = result.package_key;
    reason;
    message;
    ocamlc_warnings = result.ocamlc_warnings;
    duration_ms = Int.from_float (Time.Duration.to_secs_float result.duration *. 1_000.0);
  }

let failures_of_build_results = fun results -> List.map results ~fn:failure_of_build_result

let failure_to_json = fun (failure: failure) ->
  Data.Json.Object [
    ("package_name", Data.Json.String (Riot_model.Package_name.to_string failure.package_name));
    ("package_key", Data.Json.String (Riot_model.Package.key_to_string failure.package_key));
    ("reason", failure_reason_to_json failure.reason);
    ("message", Data.Json.String failure.message);
    ("ocamlc_warnings", Data.Json.Array (List.map failure.ocamlc_warnings ~fn:Data.Json.string));
    ("duration_ms", Data.Json.Int failure.duration_ms);
  ]

let failure_message = fun (failure: failure) ->
  Riot_model.Package_name.to_string failure.package_name ^ ": " ^ failure.message
