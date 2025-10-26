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
             ("package", Data.Json.String package.name);
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
             ("package", Data.Json.String package.name);
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
             ("package", Data.Json.String package.name);
             ( "target",
               Data.Json.String
                 (match target with
                 | Workspace_planner.All -> "all"
                 | Workspace_planner.Package pkg -> pkg) );
             ("error", Data.Json.String error);
           ])
  | ActionStarted { package; action } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "ActionStarted");
             ("package", Data.Json.String package.name);
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
             ("package", Data.Json.String package.name);
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
             ("package", Data.Json.String package.name);
             ("action_hash", Data.Json.String action_hash);
             ("error", Data.Json.String error);
           ])
  | CacheHit { package; action; hash } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "CacheHit");
             ("package", Data.Json.String package.name);
             ("action_hash", Data.Json.String action_hash);
             ("hash", Data.Json.String (Crypto.Digest.hex hash));
           ])
  | CacheMiss { package; action; hash } ->
      let action_hash = Crypto.Digest.hex (Action_node.get_hash action) in
      Some
        (Data.Json.Object
           [
             ("type", Data.Json.String "CacheMiss");
             ("package", Data.Json.String package.name);
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
      | Some (Data.Json.String "BuildStarted") ->
          Error
            (Data.Json.String "BuildStarted deserialization requires Package.t")
      | Some (Data.Json.String "BuildCompleted") ->
          Error
            (Data.Json.String
               "BuildCompleted deserialization requires Package.t")
      | Some (Data.Json.String "BuildFailed") ->
          Error
            (Data.Json.String "BuildFailed deserialization requires Package.t")
      | Some (Data.Json.String "ActionStarted") ->
          Error
            (Data.Json.String
               "ActionStarted deserialization requires Action_node.t")
      | Some (Data.Json.String "ActionCompleted") ->
          Error
            (Data.Json.String
               "ActionCompleted deserialization requires Action_node.t")
      | Some (Data.Json.String "ActionFailed") ->
          Error
            (Data.Json.String
               "ActionFailed deserialization requires Action_node.t")
      | Some (Data.Json.String "CacheHit") ->
          Error
            (Data.Json.String "CacheHit deserialization requires Action_node.t")
      | Some (Data.Json.String "CacheMiss") ->
          Error
            (Data.Json.String "CacheMiss deserialization requires Action_node.t")
      | Some (Data.Json.String "WorkspaceStarted") ->
          Error
            (Data.Json.String
               "WorkspaceStarted deserialization requires full workspace \
                context")
      | Some (Data.Json.String "WorkspaceCompleted") ->
          Error
            (Data.Json.String
               "WorkspaceCompleted deserialization requires full workspace \
                context")
      | Some (Data.Json.String typ) ->
          Error
            (Data.Json.String (format "Unknown telemetry event type: %s" typ))
      | None ->
          Error (Data.Json.String "Missing 'type' field in telemetry event")
      | _ -> Error (Data.Json.String "Invalid 'type' field in telemetry event"))
  | _ -> Error (Data.Json.String "Telemetry event must be a JSON object")
