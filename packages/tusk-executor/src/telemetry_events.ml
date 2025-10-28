open Std
open Tusk_model
open Tusk_planner
open Tusk_store

type Telemetry.event +=
  | BuildStarted of { package : Package.t; target : Workspace_planner.target }
  | BuildCompleted of {
      package : Package.t;
      target : Workspace_planner.target;
      status : [ `Fresh | `Cached ];
      duration : Time.Duration.t;
    }
  | BuildFailed of {
      package : Package.t;
      target : Workspace_planner.target;
      error : string;
    }
  | BuildSkipped of {
      package : Package.t;
      target : Workspace_planner.target;
      reason : string;
    }
  | ActionStarted of { package : Package.t; action : Action_node.t }
  | ActionCompleted of {
      package : Package.t;
      action : Action_node.t;
      artifact : Artifact.t;
      status : [ `Fresh | `Cached ];
      duration : Time.Duration.t;
    }
  | ActionFailed of {
      package : Package.t;
      action : Action_node.t;
      error : string;
    }
  | CacheHit of {
      package : Package.t;
      action : Action_node.t;
      hash : Crypto.hash;
    }
  | CacheMiss of {
      package : Package.t;
      action : Action_node.t;
      hash : Crypto.hash;
    }
  | WorkspaceStarted of {
      target : Workspace_planner.target;
      package_count : int;
    }
  | WorkspaceCompleted of {
      target : Workspace_planner.target;
      total_duration : Time.Duration.t;
      cached_count : int;
      built_count : int;
      failed_count : int;
    }

let to_json : Telemetry.event -> Data.Json.t option = function
  | BuildStarted { package; target } ->
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "BuildStarted");
             ("package", Package.to_json package);
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
           ])
  | BuildCompleted { package; target; status; duration } ->
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "BuildCompleted");
             ("package", Package.to_json package);
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
             ( "status",
               Data.Json.String
                 (match status with `Fresh -> "fresh" | `Cached -> "cached") );
             ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
           ])
  | BuildFailed { package; target; error } ->
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "BuildFailed");
             ("package", Package.to_json package);
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
             ("error", Data.Json.String error);
           ])
  | BuildSkipped { package; target; reason } ->
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "BuildSkipped");
             ("package", Package.to_json package);
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
             ("reason", Data.Json.String reason);
           ])
  | ActionStarted { package; action } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "ActionStarted");
             ("package", Package.to_json package);
             ("action_hash", Data.Json.String action_hash);
           ])
  | ActionCompleted { package; action; artifact; status; duration } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      let artifact_files =
        Data.Json.Array
          (List.map
             (fun p -> Data.Json.String (Path.to_string p))
             artifact.files)
      in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "ActionCompleted");
             ("package", Package.to_json package);
             ("action_hash", Data.Json.String action_hash);
             ("artifact_files", artifact_files);
             ( "status",
               Data.Json.String
                 (match status with `Fresh -> "fresh" | `Cached -> "cached") );
             ("duration_ms", Data.Json.Int (Time.Duration.to_millis duration));
           ])
  | ActionFailed { package; action; error } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "ActionFailed");
             ("package", Package.to_json package);
             ("action_hash", Data.Json.String action_hash);
             ("error", Data.Json.String error);
           ])
  | CacheHit { package; action; hash } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "CacheHit");
             ("package", Package.to_json package);
             ("action_hash", Data.Json.String action_hash);
             ("hash", Data.Json.String (Crypto.Digest.hex hash));
           ])
  | CacheMiss { package; action; hash } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "CacheMiss");
             ("package", Package.to_json package);
             ("action_hash", Data.Json.String action_hash);
             ("hash", Data.Json.String (Crypto.Digest.hex hash));
           ])
  | WorkspaceStarted { target; package_count } ->
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "WorkspaceStarted");
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
             ("package_count", Data.Json.Int package_count);
           ])
  | WorkspaceCompleted
      { target; total_duration; cached_count; built_count; failed_count } ->
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "WorkspaceCompleted");
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
             ( "total_duration_ms",
               Data.Json.Int (Time.Duration.to_millis total_duration) );
             ("cached_count", Data.Json.Int cached_count);
             ("built_count", Data.Json.Int built_count);
             ("failed_count", Data.Json.Int failed_count);
           ])
  | _ -> None

let from_json (json : Data.Json.t) : (Telemetry.event, Data.Json.t) result =
  match json with
  | Data.Json.Object fields -> (
      match List.assoc_opt "type" fields with
      | Some (Data.Json.String "BuildStarted") -> (
          match
            (List.assoc_opt "package" fields, List.assoc_opt "target" fields)
          with
          | Some package_json, Some (Data.Json.String target_str) -> (
              match Package.from_json package_json with
              | Ok package ->
                  let target =
                    match target_str with
                    | "all" -> Workspace_planner.All
                    | pkg -> Workspace_planner.Package pkg
                  in
                  Ok (BuildStarted { package; target })
              | Error e -> Error (Data.Json.String e))
          | _ -> Error (Data.Json.String "Invalid BuildStarted event"))
      | Some (Data.Json.String "BuildCompleted") -> (
          match
            ( List.assoc_opt "package" fields,
              List.assoc_opt "target" fields,
              List.assoc_opt "status" fields,
              List.assoc_opt "duration_ms" fields )
          with
          | ( Some package_json,
              Some (Data.Json.String target_str),
              Some (Data.Json.String status_str),
              Some (Data.Json.Int duration_ms) ) -> (
              match Package.from_json package_json with
              | Ok package ->
                  let target =
                    match target_str with
                    | "all" -> Workspace_planner.All
                    | pkg -> Workspace_planner.Package pkg
                  in
                  let status =
                    match status_str with "cached" -> `Cached | _ -> `Fresh
                  in
                  let duration = Time.Duration.from_millis duration_ms in
                  Ok (BuildCompleted { package; target; status; duration })
              | Error e -> Error (Data.Json.String e))
          | _ -> Error (Data.Json.String "Invalid BuildCompleted event"))
      | Some (Data.Json.String "BuildFailed") -> (
          match
            ( List.assoc_opt "package" fields,
              List.assoc_opt "target" fields,
              List.assoc_opt "error" fields )
          with
          | ( Some package_json,
              Some (Data.Json.String target_str),
              Some (Data.Json.String error) ) -> (
              match Package.from_json package_json with
              | Ok package ->
                  let target =
                    match target_str with
                    | "all" -> Workspace_planner.All
                    | pkg -> Workspace_planner.Package pkg
                  in
                  Ok (BuildFailed { package; target; error })
              | Error e -> Error (Data.Json.String e))
          | _ -> Error (Data.Json.String "Invalid BuildFailed event"))
      | Some (Data.Json.String "BuildSkipped") -> (
          match
            ( List.assoc_opt "package" fields,
              List.assoc_opt "target" fields,
              List.assoc_opt "reason" fields )
          with
          | ( Some package_json,
              Some (Data.Json.String target_str),
              Some (Data.Json.String reason) ) -> (
              match Package.from_json package_json with
              | Ok package ->
                  let target =
                    match target_str with
                    | "all" -> Workspace_planner.All
                    | pkg -> Workspace_planner.Package pkg
                  in
                  Ok (BuildSkipped { package; target; reason })
              | Error e -> Error (Data.Json.String e))
          | _ -> Error (Data.Json.String "Invalid BuildSkipped event"))
      | Some (Data.Json.String "WorkspaceStarted") -> (
          match
            ( List.assoc_opt "target" fields,
              List.assoc_opt "package_count" fields )
          with
          | ( Some (Data.Json.String target_str),
              Some (Data.Json.Int package_count) ) ->
              let target =
                match target_str with
                | "all" -> Workspace_planner.All
                | pkg -> Workspace_planner.Package pkg
              in
              Ok (WorkspaceStarted { target; package_count })
          | _ -> Error (Data.Json.String "Invalid WorkspaceStarted event"))
      | Some (Data.Json.String "WorkspaceCompleted") -> (
          match
            ( List.assoc_opt "target" fields,
              List.assoc_opt "total_duration_ms" fields,
              List.assoc_opt "cached_count" fields,
              List.assoc_opt "built_count" fields,
              List.assoc_opt "failed_count" fields )
          with
          | ( Some (Data.Json.String target_str),
              Some (Data.Json.Int total_duration_ms),
              Some (Data.Json.Int cached_count),
              Some (Data.Json.Int built_count),
              Some (Data.Json.Int failed_count) ) ->
              let target =
                match target_str with
                | "all" -> Workspace_planner.All
                | pkg -> Workspace_planner.Package pkg
              in
              let total_duration =
                Time.Duration.from_millis total_duration_ms
              in
              Ok
                (WorkspaceCompleted
                   {
                     target;
                     total_duration;
                     cached_count;
                     built_count;
                     failed_count;
                   })
          | _ -> Error (Data.Json.String "Invalid WorkspaceCompleted event"))
      (* Action events still can't be deserialized - they need Action_node.t *)
      | Some (Data.Json.String "ActionStarted")
      | Some (Data.Json.String "ActionCompleted")
      | Some (Data.Json.String "ActionFailed")
      | Some (Data.Json.String "CacheHit")
      | Some (Data.Json.String "CacheMiss") ->
          Error
            (Data.Json.String
               "Action events require Action_node.t (not serializable)")
      | Some (Data.Json.String typ) ->
          Error
            (Data.Json.String (format "Unknown telemetry event type: %s" typ))
      | None ->
          Error (Data.Json.String "Missing 'type' field in telemetry event")
      | _ -> Error (Data.Json.String "Invalid 'type' field in telemetry event"))
  | _ -> Error (Data.Json.String "Telemetry event must be a JSON object")
