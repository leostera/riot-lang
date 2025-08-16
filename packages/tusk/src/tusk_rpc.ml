(** Tusk RPC Protocol - JSON-RPC 2.0 compliant *)

(** Method names *)
let method_ping = "tusk.ping"
let method_get_build_graph = "tusk.getBuildGraph"
let method_get_workspace_config = "tusk.getWorkspaceConfig"
let method_build_package = "tusk.buildPackage"
let method_build_all = "tusk.buildAll"
let method_restart = "tusk.restart"
let method_shutdown = "tusk.shutdown"

(** Response types *)
type build_node = {
  package_name : string;
  src_dir : string;
  out_dir : string;
  status : string;
  deps : string list;
}

type build_graph_response = { 
  nodes : build_node list 
}

type workspace_config = {
  workspace_root : string;
  toolchain : string;
  packages : string list;
}

type build_started = {
  session_id : string;
}

type log_output = {
  session_id : string;
  message : string;
}

(** Convert build node to/from JSON *)
let build_node_to_json node =
  Json.Object [
    ("package_name", Json.String node.package_name);
    ("src_dir", Json.String node.src_dir);
    ("out_dir", Json.String node.out_dir);
    ("status", Json.String node.status);
    ("deps", Json.Array (List.map (fun s -> Json.String s) node.deps));
  ]

let build_node_of_json json =
  match json with
  | Json.Object fields -> (
      match 
        List.assoc_opt "package_name" fields,
        List.assoc_opt "src_dir" fields,
        List.assoc_opt "out_dir" fields,
        List.assoc_opt "status" fields,
        List.assoc_opt "deps" fields
      with
      | Some (Json.String package_name),
        Some (Json.String src_dir),
        Some (Json.String out_dir),
        Some (Json.String status),
        Some (Json.Array deps_json) ->
          let deps_result = List.fold_left (fun acc d ->
            match acc, d with
            | Ok deps, Json.String dep -> Ok (dep :: deps)
            | Error e, _ -> Error e
            | _, _ -> Error "Invalid dependency in deps array"
          ) (Ok []) deps_json in
          (match deps_result with
          | Ok deps -> Ok { package_name; src_dir; out_dir; status; deps = List.rev deps }
          | Error e -> Error e)
      | _ -> Error "Invalid build_node structure")
  | _ -> Error "build_node must be an object"

(** Convert build graph to/from JSON *)
let build_graph_to_json graph =
  Json.Object [
    ("nodes", Json.Array (List.map build_node_to_json graph.nodes))
  ]

let build_graph_of_json json =
  match json with
  | Json.Object fields -> (
      match List.assoc_opt "nodes" fields with
      | Some (Json.Array nodes_json) ->
          let nodes_result = List.fold_left (fun acc node ->
            match acc, build_node_of_json node with
            | Ok nodes, Ok n -> Ok (n :: nodes)
            | Error e, _ -> Error e
            | _, Error e -> Error e
          ) (Ok []) nodes_json in
          (match nodes_result with
          | Ok nodes -> Ok { nodes = List.rev nodes }
          | Error e -> Error e)
      | _ -> Error "Invalid build_graph structure")
  | _ -> Error "build_graph must be an object"

(** Convert workspace config to/from JSON *)
let workspace_config_to_json config =
  Json.Object [
    ("workspace_root", Json.String config.workspace_root);
    ("toolchain", Json.String config.toolchain);
    ("packages", Json.Array (List.map (fun s -> Json.String s) config.packages));
  ]

let workspace_config_of_json json =
  match json with
  | Json.Object fields -> (
      match 
        List.assoc_opt "workspace_root" fields,
        List.assoc_opt "toolchain" fields,
        List.assoc_opt "packages" fields
      with
      | Some (Json.String workspace_root),
        Some (Json.String toolchain),
        Some (Json.Array packages_json) ->
          let packages_result = List.fold_left (fun acc p ->
            match acc, p with
            | Ok pkgs, Json.String pkg -> Ok (pkg :: pkgs)
            | Error e, _ -> Error e
            | _, _ -> Error "Invalid package in packages array"
          ) (Ok []) packages_json in
          (match packages_result with
          | Ok packages -> Ok { workspace_root; toolchain; packages = List.rev packages }
          | Error e -> Error e)
      | _ -> Error "Invalid workspace_config structure")
  | _ -> Error "workspace_config must be an object"

(** Convert build started to/from JSON *)
let build_started_to_json bs =
  Json.Object [
    ("session_id", Json.String bs.session_id)
  ]

let build_started_of_json json =
  match json with
  | Json.Object fields -> (
      match List.assoc_opt "session_id" fields with
      | Some (Json.String session_id) -> Ok { session_id }
      | _ -> Error "Invalid build_started structure")
  | _ -> Error "build_started must be an object"

(** Convert log output to/from JSON *)
let log_output_to_json lo =
  Json.Object [
    ("session_id", Json.String lo.session_id);
    ("message", Json.String lo.message);
  ]

let log_output_of_json json =
  match json with
  | Json.Object fields -> (
      match 
        List.assoc_opt "session_id" fields,
        List.assoc_opt "message" fields
      with
      | Some (Json.String session_id), Some (Json.String message) ->
          Ok { session_id; message }
      | _ -> Error "Invalid log_output structure")
  | _ -> Error "log_output must be an object"

(** Helper to create method-specific parameters *)
let build_package_params package =
  Jsonrpc.Named [("package", Json.String package)]