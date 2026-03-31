open Std
open Std.Collections
open Tusk_model
open Tusk_planner
open Tusk_store

(** Error types for package builds *)
type package_error =
  | PlanningFailed of Planning_error.t
  | ExecutionFailed of { message : string; }
  | ActionExecutionFailed of { message : string; }
  | ActionOutputsNotCreated of { missing : Path.t list; }
  | ActionDependenciesFailed of { failed : Graph.SimpleGraph.Node_id.t list; }

type package_planning_status =
[
  | `Planned
  | `MissingDependencies
  | `FailedDependencies
  | `Failed
]

type Telemetry.event +=
  | BuildStarted of {
      session_id : Session_id.t;
      package : Package.t;
      target : Workspace_planner.target;
    }
  | PlanningWorkspaceStarted of {
      session_id : Session_id.t;
      target : Workspace_planner.target;
      package_count : int;
    }
  | PlanningWorkspaceCompleted of {
      session_id : Session_id.t;
      target : Workspace_planner.target;
      duration : Time.Duration.t;
      planned_count : int;
      missing_count : int;
      failed_count : int;
    }
  | PackagePlanningResult of {
      session_id : Session_id.t;
      package : Package.t;
      target : Workspace_planner.target;
      status : package_planning_status;
      duration : Time.Duration.t;
      reason : string option;
    }
  | CompilationStarted of {
      session_id : Session_id.t;
      package : Package.t;
      target : Workspace_planner.target;
    }
  | BuildCompleted of {
      session_id : Session_id.t;
      package : Package.t;
      target : Workspace_planner.target;
      status :
        [
          `Fresh
          | `Cached
        ];
      duration : Time.Duration.t;
    }
  | BuildFailed of {
      session_id : Session_id.t;
      package : Package.t;
      target : Workspace_planner.target;
      error : package_error;
    }
  | BuildSkipped of {
      session_id : Session_id.t;
      package : Package.t;
      target : Workspace_planner.target;
      reason : string;
    }
  | ActionStarted of {
      session_id : Session_id.t;
      package : Package.t;
      action : Action_node.t;
    }
  | ActionCommandStarted of {
      session_id : Session_id.t;
      package : Package.t;
      action : Action_node.t;
      command : string;
    }
  | ActionCompleted of {
      session_id : Session_id.t;
      package : Package.t;
      action : Action_node.t;
      artifact : Artifact.t;
      status :
        [
          `Fresh
          | `Cached
        ];
      duration : Time.Duration.t;
    }
  | ActionFailed of {
      session_id : Session_id.t;
      package : Package.t;
      action : Action_node.t;
      error : string;
    }
  | CacheHit of {
      session_id : Session_id.t;
      package : Package.t;
      action : Action_node.t;
      hash : Crypto.hash;
    }
  | CacheMiss of {
      session_id : Session_id.t;
      package : Package.t;
      action : Action_node.t;
      hash : Crypto.hash;
    }
  | WorkspaceStarted of {
      session_id : Session_id.t;
      target : Workspace_planner.target;
      package_count : int;
    }
  | WorkspaceCompleted of {
      session_id : Session_id.t;
      target : Workspace_planner.target;
      total_duration : Time.Duration.t;
      cached_count : int;
      built_count : int;
      failed_count : int;
    }

let target_to_json = fun target ->
  Data.Json.String (
    match target with
    | Workspace_planner.All -> "all"
    | Workspace_planner.Package pkg -> pkg
    | Workspace_planner.Packages pkgs -> "packages:" ^ String.concat "," pkgs
  )

let target_of_json =
  function
  | Data.Json.String "all" ->
      Ok Workspace_planner.All
  | Data.Json.String target_str when String.starts_with ~prefix:"packages:" target_str ->
      let prefix_len = String.length "packages:" in
      let packages_str = String.sub target_str prefix_len (String.length target_str - prefix_len) in
      let packages =
        if String.equal packages_str "" then
          []
        else
          String.split_on_char ',' packages_str
      in
      Ok (Workspace_planner.Packages packages)
  | Data.Json.String pkg ->
      Ok (Workspace_planner.Package pkg)
  | _ ->
      Error (Data.Json.String "Invalid target")

let action_to_json = fun (action:Action_node.t) ->
  let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
  Data.Json.Object [
    ("action_hash", Data.Json.String action_hash);
    ("action_node", Action_node.to_json action);

  ]

let planning_status_to_json =
  function
  | `Planned -> Data.Json.String "planned"
  | `MissingDependencies -> Data.Json.String "missing_dependencies"
  | `FailedDependencies -> Data.Json.String "failed_dependencies"
  | `Failed -> Data.Json.String "failed"

let planning_status_of_json =
  function
  | Data.Json.String "planned" -> Ok `Planned
  | Data.Json.String "missing_dependencies" -> Ok `MissingDependencies
  | Data.Json.String "failed_dependencies" -> Ok `FailedDependencies
  | Data.Json.String "failed" -> Ok `Failed
  | _ -> Error (Data.Json.String "Invalid planning status")

let to_json : Telemetry.event -> Data.Json.t option =
  function
  | BuildStarted { session_id; package; target } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);

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

          ]
          @ match reason with
          | Some reason -> [ ("reason", Data.Json.String reason) ]
          | None -> []
        )
      )
  | CompilationStarted { session_id; package; target } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "CompilationStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);

      ])
  | BuildCompleted {
    session_id;
    package;
    target;
    status;
    duration
  } ->
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "BuildCompleted");
          ("session_id", Data.Json.String (Session_id.to_string session_id));
          ("package", Package.to_json package);
          ("target", target_to_json target);
          (
            "status",
            Data.Json.String (
              match status with
              | `Fresh -> "fresh"
              | `Cached -> "cached"
            )
          );
          ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));

        ]
      )
  | BuildFailed { session_id; package; target; error } ->
      let error_json =
        match error with
        | PlanningFailed planning_err -> Data.Json.Object [
          ("type", Data.Json.String "planning_failed");
          ("error", Planning_error.to_json planning_err);

        ]
        | ExecutionFailed { message } -> Data.Json.Object [
          ("type", Data.Json.String "execution_failed");
          ("message", Data.Json.String message);

        ]
        | ActionExecutionFailed { message } -> Data.Json.Object [
          ("type", Data.Json.String "action_failed");
          ("message", Data.Json.String message);

        ]
        | ActionOutputsNotCreated { missing } -> Data.Json.Object [
          ("type", Data.Json.String "outputs_not_created");
          (
            "missing",
            Data.Json.Array (List.map (fun p -> Data.Json.String (Path.to_string p)) missing)
          );

        ]
        | ActionDependenciesFailed { failed } -> Data.Json.Object [
          ("type", Data.Json.String "dependencies_failed");
          ("failed_count", Data.Json.String (Int.to_string (List.length failed)));

        ]
      in
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildFailed");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("error", error_json);

      ])
  | BuildSkipped { session_id; package; target; reason } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "BuildSkipped");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("target", target_to_json target);
        ("reason", Data.Json.String reason);

      ])
  | ActionStarted { session_id; package; action } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "ActionStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("action", action_to_json action);

      ])
  | ActionCommandStarted { session_id; package; action; command } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "ActionCommandStarted");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("action", action_to_json action);
        ("command", Data.Json.String command);

      ])
  | ActionCompleted {
    session_id;
    package;
    action;
    artifact;
    status;
    duration
  } ->
      let artifact_files = Data.Json.Array (List.map
      (fun p -> Data.Json.String (Path.to_string p))
      artifact.files) in
      Some (
        Data.Json.Object [
          ("type", Data.Json.String "ActionCompleted");
          ("session_id", Data.Json.String (Session_id.to_string session_id));
          ("package", Package.to_json package);
          ("action", action_to_json action);
          ("artifact_files", artifact_files);
          (
            "status",
            Data.Json.String (
              match status with
              | `Fresh -> "fresh"
              | `Cached -> "cached"
            )
          );
          ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));

        ]
      )
  | ActionFailed { session_id; package; action; error } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "ActionFailed");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("action", action_to_json action);
        ("error", Data.Json.String error);

      ])
  | CacheHit { session_id; package; action; hash } ->
      Some (Data.Json.Object [
        ("type", Data.Json.String "CacheHit");
        ("session_id", Data.Json.String (Session_id.to_string session_id));
        ("package", Package.to_json package);
        ("action", action_to_json action);
        ("hash", Data.Json.String (Crypto.Digest.hex hash));

      ])
  | CacheMiss { session_id; package; action; hash } ->
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
    failed_count
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
  | _ ->
      None

let from_json : Data.Json.t -> (Telemetry.event, Data.Json.t) result = fun json ->
  let get_session_id = fun fields ->
    match List.assoc_opt "session_id" fields with
    | Some (Data.Json.String sid) -> Session_id.of_string sid
    | _ -> Session_id.of_string "unknown"
  in
  match json with
  | Data.Json.Object fields -> (
      match List.assoc_opt "type" fields with
      | Some (Data.Json.String "BuildStarted") -> (
          match (
            List.assoc_opt "session_id" fields,
            List.assoc_opt "package" fields,
            List.assoc_opt "target" fields
          ) with
          | Some (Data.Json.String session_id_str), Some package_json, Some target_json -> (
              match Package.from_json package_json with
              | Ok package ->
                  let session_id = Session_id.of_string session_id_str in
                  (
                    match target_of_json target_json with
                    | Ok target -> Ok (BuildStarted {session_id; package; target})
                    | Error e -> Error e
                  )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildStarted event")
        )
      | Some (Data.Json.String "PlanningWorkspaceStarted") -> (
          match (List.assoc_opt "target" fields, List.assoc_opt "package_count" fields) with
          | Some (Data.Json.String target_str), Some (Data.Json.Int package_count) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target -> Ok (PlanningWorkspaceStarted {
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
            List.assoc_opt "target" fields,
            List.assoc_opt "duration_ms" fields,
            List.assoc_opt "planned_count" fields,
            List.assoc_opt "missing_count" fields,
            List.assoc_opt "failed_count" fields
          ) with
          | (Some (Data.Json.String target_str), Some (Data.Json.Int duration_ms), Some (Data.Json.Int planned_count), Some (Data.Json.Int missing_count), Some (Data.Json.Int failed_count)) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target -> Ok (PlanningWorkspaceCompleted {
                session_id = get_session_id fields;
                target;
                duration = Time.Duration.from_millis duration_ms;
                planned_count;
                missing_count;
                failed_count;

              })
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid PlanningWorkspaceCompleted event")
        )
      | Some (Data.Json.String "PackagePlanningResult") -> (
          match (
            List.assoc_opt "package" fields,
            List.assoc_opt "target" fields,
            List.assoc_opt "status" fields,
            List.assoc_opt "duration_ms" fields
          ) with
          | (Some package_json, Some (Data.Json.String target_str), Some status_json, Some (Data.Json.Int duration_ms)) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match (
                    target_of_json (Data.Json.String target_str),
                    planning_status_of_json status_json
                  ) with
                  | Ok target, Ok status ->
                      Ok (
                        PackagePlanningResult {
                          session_id = get_session_id fields;
                          package;
                          target;
                          status;
                          duration = Time.Duration.from_millis duration_ms;
                          reason = (
                            match List.assoc_opt "reason" fields with
                            | Some (Data.Json.String reason) -> Some reason
                            | _ -> None
                          );

                        }
                      )
                  | Error e, _ -> Error e
                  | _, Error e -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid PackagePlanningResult event")
        )
      | Some (Data.Json.String "CompilationStarted") -> (
          match (
            List.assoc_opt "session_id" fields,
            List.assoc_opt "package" fields,
            List.assoc_opt "target" fields
          ) with
          | Some (Data.Json.String session_id_str), Some package_json, Some target_json -> (
              match Package.from_json package_json with
              | Ok package ->
                  let session_id = Session_id.of_string session_id_str in
                  (
                    match target_of_json target_json with
                    | Ok target -> Ok (CompilationStarted {session_id; package; target})
                    | Error e -> Error e
                  )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid CompilationStarted event")
        )
      | Some (Data.Json.String "BuildCompleted") -> (
          match (
            List.assoc_opt "package" fields,
            List.assoc_opt "target" fields,
            List.assoc_opt "status" fields,
            List.assoc_opt "duration_ms" fields
          ) with
          | (Some package_json, Some target_json, Some (Data.Json.String status_str), Some (Data.Json.Int duration_ms)) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match target_of_json target_json with
                  | Ok target ->
                      let status =
                        match status_str with
                        | "cached" -> `Cached
                        | _ -> `Fresh
                      in
                      let duration = Time.Duration.from_millis duration_ms in
                      Ok (BuildCompleted {
                        session_id = get_session_id fields;
                        package;
                        target;
                        status;
                        duration
                      })
                  | Error e -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildCompleted event")
        )
      | Some (Data.Json.String "BuildFailed") -> (
          match (
            List.assoc_opt "package" fields,
            List.assoc_opt "target" fields,
            List.assoc_opt "error" fields
          ) with
          | (Some package_json, Some target_json, Some error_json) -> (
              match Package.from_json package_json with
              | Ok package ->
                  (* Deserialize structured error *)
                  let error_result =
                    match error_json with
                    | Data.Json.Object error_fields -> (
                        match List.assoc_opt "type" error_fields with
                        | Some (Data.Json.String "planning_failed") -> (
                            (* For planning errors, try to deserialize from the nested error field *)
                            match List.assoc_opt "error" error_fields with
                            | Some (Data.Json.Object planning_fields) -> (
                                match List.assoc_opt "type" planning_fields with
                                | Some (Data.Json.String "cyclic_dependency") -> (
                                    match List.assoc_opt "cycle" planning_fields with
                                    | Some (Data.Json.Array arr) ->
                                        let cycle =
                                          List.filter_map
                                            (
                                              function
                                              | Data.Json.String s -> Some s
                                              | _ -> None
                                            )
                                            arr
                                        in
                                        Ok (PlanningFailed (Planning_error.CyclicDependency {cycle}))
                                    | _ -> Ok (ExecutionFailed {
                                      message = "Planning failed: cyclic dependency";

                                    })
                                  )
                                | Some (Data.Json.String "scan_failed") -> (
                                    match (
                                      List.assoc_opt "path" planning_fields,
                                      List.assoc_opt "reason" planning_fields
                                    ) with
                                    | (Some (Data.Json.String path), Some (Data.Json.String reason)) -> Ok (PlanningFailed (Planning_error.ScanFailed {
                                      path = Path.v path;
                                      reason
                                    }))
                                    | _ -> Ok (ExecutionFailed {
                                      message = "Planning failed: scan failed"
                                    })
                                  )
                                | Some (Data.Json.String "dependency_analysis_failed") -> (
                                    match List.assoc_opt "reason" planning_fields with
                                    | Some (Data.Json.String reason) -> Ok (PlanningFailed (Planning_error.DependencyAnalysisFailed {
                                      reason
                                    }))
                                    | _ -> Ok (ExecutionFailed {
                                      message = "Planning failed: dependency analysis failed";

                                    })
                                  )
                                | Some (Data.Json.String "graph_build_failed") -> (
                                    match List.assoc_opt "reason" planning_fields with
                                    | Some (Data.Json.String reason) -> Ok (PlanningFailed (Planning_error.GraphBuildFailed {
                                      reason
                                    }))
                                    | _ -> Ok (ExecutionFailed {
                                      message = "Planning failed: graph build failed";

                                    })
                                  )
                                | Some (Data.Json.String "exception") -> (
                                    match List.assoc_opt "message" planning_fields with
                                    | Some (Data.Json.String msg) -> Ok (PlanningFailed (Planning_error.Exception {
                                      exn = Failure msg
                                    }))
                                    | _ -> Ok (ExecutionFailed {
                                      message = "Planning failed: exception"
                                    })
                                  )
                                | _ ->
                                    Ok (ExecutionFailed {
                                      message = "Planning failed: unknown planning error";

                                    })
                              )
                            | _ -> Ok (ExecutionFailed {
                              message = "Planning failed: missing error details"
                            })
                          )
                        | Some (Data.Json.String "execution_failed") -> (
                            match List.assoc_opt "message" error_fields with
                            | Some (Data.Json.String msg) -> Ok (ExecutionFailed {message = msg})
                            | _ -> Ok (ExecutionFailed {
                              message = "Execution failed: missing message"
                            })
                          )
                        | Some (Data.Json.String "action_failed") -> (
                            match List.assoc_opt "message" error_fields with
                            | Some (Data.Json.String msg) -> Ok (ActionExecutionFailed {
                              message = msg
                            })
                            | _ -> Ok (ExecutionFailed {message = "Action failed: missing message"})
                          )
                        | Some (Data.Json.String "outputs_not_created") -> (
                            match List.assoc_opt "missing" error_fields with
                            | Some (Data.Json.Array arr) ->
                                let missing =
                                  List.filter_map
                                    (
                                      function
                                      | Data.Json.String s -> Some (Path.v s)
                                      | _ -> None
                                    )
                                    arr
                                in
                                Ok (ActionOutputsNotCreated {missing})
                            | _ -> Ok (ExecutionFailed {
                              message = "Outputs not created: missing list"
                            })
                          )
                        | Some (Data.Json.String "dependencies_failed") ->
                            Ok (ActionDependenciesFailed {failed = []})
                        | _ ->
                            Ok (ExecutionFailed {message = "Unknown error type"})
                      )
                    | _ -> Ok (ExecutionFailed {message = "Invalid error format"})
                  in
                  (
                    match (target_of_json target_json, error_result) with
                    | Ok target, Ok error -> Ok (BuildFailed {
                      session_id = get_session_id fields;
                      package;
                      target;
                      error
                    })
                    | Error e, _ -> Error e
                    | _, Error e -> Error e
                  )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildFailed event")
        )
      | Some (Data.Json.String "BuildSkipped") -> (
          match (
            List.assoc_opt "package" fields,
            List.assoc_opt "target" fields,
            List.assoc_opt "reason" fields
          ) with
          | (Some package_json, Some target_json, Some (Data.Json.String reason)) -> (
              match Package.from_json package_json with
              | Ok package -> (
                  match target_of_json target_json with
                  | Ok target -> Ok (BuildSkipped {
                    session_id = get_session_id fields;
                    package;
                    target;
                    reason
                  })
                  | Error e -> Error e
                )
              | Error e -> Error (Data.Json.String e)
            )
          | _ -> Error (Data.Json.String "Invalid BuildSkipped event")
        )
      | Some (Data.Json.String "WorkspaceStarted") -> (
          match (List.assoc_opt "target" fields, List.assoc_opt "package_count" fields) with
          | (Some (Data.Json.String target_str), Some (Data.Json.Int package_count)) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target -> Ok (WorkspaceStarted {
                session_id = get_session_id fields;
                target;
                package_count
              })
              | Error e -> Error e
            )
          | _ -> Error (Data.Json.String "Invalid WorkspaceStarted event")
        )
      | Some (Data.Json.String "WorkspaceCompleted") -> (
          match (
            List.assoc_opt "target" fields,
            List.assoc_opt "total_duration_ms" fields,
            List.assoc_opt "cached_count" fields,
            List.assoc_opt "built_count" fields,
            List.assoc_opt "failed_count" fields
          ) with
          | (Some (Data.Json.String target_str), Some (Data.Json.Int total_duration_ms), Some (Data.Json.Int cached_count), Some (Data.Json.Int built_count), Some (Data.Json.Int failed_count)) -> (
              match target_of_json (Data.Json.String target_str) with
              | Ok target ->
                  let total_duration = Time.Duration.from_millis total_duration_ms in
                  Ok (WorkspaceCompleted {
                    session_id = get_session_id fields;
                    target;
                    total_duration;
                    cached_count;
                    built_count;
                    failed_count;

                  })
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
      | None ->
          Error (Data.Json.String "Missing 'type' field in telemetry event")
      | _ ->
          Error (Data.Json.String "Invalid 'type' field in telemetry event")
    )
  | _ -> Error (Data.Json.String "Telemetry event must be a JSON object")
