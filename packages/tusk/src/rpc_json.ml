(** JSON-based RPC protocol for tusk server *)

type build_node = {
  package_name : string;
  src_dir : string;
  out_dir : string;
  status : string;
  deps : string list;
}

type build_graph_response = { nodes : build_node list }

type workspace_config = {
  workspace_root : string;
  toolchain : string;
  packages : string list;
}

type request =
  | Ping
  | GetBuildGraph
  | GetWorkspaceConfig
  | BuildPackage of string
  | BuildAll
  | Restart
  | Shutdown

type response =
  | Pong
  | BuildGraph of build_graph_response
  | WorkspaceConfig of workspace_config
  | Error of string
  | Success

(** Convert build node to JSON *)
let build_node_to_json node =
  Json.obj
    [
      ("package_name", Json.string node.package_name);
      ("src_dir", Json.string node.src_dir);
      ("out_dir", Json.string node.out_dir);
      ("status", Json.string node.status);
      ("deps", Json.array (List.map Json.string node.deps));
    ]

(** Convert JSON to build node *)
let build_node_of_json json =
  match Json.get_object json with
  | None -> Result.Error "Expected object for build_node"
  | Some obj -> (
      let json_obj = Json.Object obj in
      match
        ( Json.get_field "package_name" json_obj,
          Json.get_field "src_dir" json_obj,
          Json.get_field "out_dir" json_obj,
          Json.get_field "status" json_obj,
          Json.get_field "deps" json_obj )
      with
      | Some pn, Some sd, Some od, Some st, Some deps -> (
          match
            ( Json.get_string pn,
              Json.get_string sd,
              Json.get_string od,
              Json.get_string st,
              Json.get_array deps )
          with
          | ( Some package_name,
              Some src_dir,
              Some out_dir,
              Some status,
              Some deps_array ) -> (
              let deps_result =
                List.fold_left
                  (fun acc dep ->
                    match acc with
                    | Result.Error e -> Result.Error e
                    | Result.Ok deps_list -> (
                        match Json.get_string dep with
                        | Some s -> Result.Ok (s :: deps_list)
                        | None ->
                            Result.Error "Invalid dependency in deps array"))
                  (Result.Ok []) deps_array
              in
              match deps_result with
              | Result.Ok deps ->
                  Result.Ok
                    {
                      package_name;
                      src_dir;
                      out_dir;
                      status;
                      deps = List.rev deps;
                    }
              | Result.Error e -> Result.Error e)
          | _ -> Result.Error "Invalid field types in build_node")
      | _ -> Result.Error "Missing required fields in build_node")

(** Serialize request to JSON *)
let request_to_json = function
  | Ping -> Json.obj [ ("type", Json.string "Ping") ]
  | GetBuildGraph -> Json.obj [ ("type", Json.string "GetBuildGraph") ]
  | GetWorkspaceConfig ->
      Json.obj [ ("type", Json.string "GetWorkspaceConfig") ]
  | BuildPackage pkg ->
      Json.obj
        [ ("type", Json.string "BuildPackage"); ("package", Json.string pkg) ]
  | BuildAll -> Json.obj [ ("type", Json.string "BuildAll") ]
  | Restart -> Json.obj [ ("type", Json.string "Restart") ]
  | Shutdown -> Json.obj [ ("type", Json.string "Shutdown") ]

(** Serialize response to JSON *)
let response_to_json = function
  | Pong -> Json.obj [ ("type", Json.string "Pong") ]
  | BuildGraph graph ->
      Json.obj
        [
          ("type", Json.string "BuildGraph");
          ("nodes", Json.array (List.map build_node_to_json graph.nodes));
        ]
  | WorkspaceConfig config ->
      Json.obj
        [
          ("type", Json.string "WorkspaceConfig");
          ("workspace_root", Json.string config.workspace_root);
          ("toolchain", Json.string config.toolchain);
          ("packages", Json.array (List.map Json.string config.packages));
        ]
  | Error msg ->
      Json.obj [ ("type", Json.string "Error"); ("message", Json.string msg) ]
  | Success -> Json.obj [ ("type", Json.string "Success") ]

(** Deserialize request from JSON *)
let request_of_json json =
  match Json.get_field "type" json with
  | Some type_field -> (
      match Json.get_string type_field with
      | Some "Ping" -> Ok Ping
      | Some "GetBuildGraph" -> Ok GetBuildGraph
      | Some "GetWorkspaceConfig" -> Ok GetWorkspaceConfig
      | Some "BuildPackage" -> (
          match Json.get_field "package" json with
          | Some pkg_field -> (
              match Json.get_string pkg_field with
              | Some pkg -> Ok (BuildPackage pkg)
              | None -> Error "Invalid package field")
          | None -> Error "Missing package field for BuildPackage")
      | Some "BuildAll" -> Ok BuildAll
      | Some "Restart" -> Ok Restart
      | Some "Shutdown" -> Ok Shutdown
      | Some t -> Error (Printf.sprintf "Unknown request type: %s" t)
      | None -> Error "Type field is not a string")
  | None -> Error "Missing type field in request"

(** Deserialize response from JSON *)
let response_of_json json =
  match Json.get_field "type" json with
  | Some type_field -> (
      match Json.get_string type_field with
      | Some "Pong" -> Ok Pong
      | Some "BuildGraph" -> (
          match Json.get_field "nodes" json with
          | Some nodes_field -> (
              match Json.get_array nodes_field with
              | Some nodes_array -> (
                  let nodes_result =
                    List.fold_left
                      (fun acc node ->
                        match acc with
                        | Result.Error e -> Result.Error e
                        | Result.Ok nodes_list -> (
                            match build_node_of_json node with
                            | Result.Ok n -> Result.Ok (n :: nodes_list)
                            | Result.Error e -> Result.Error e))
                      (Result.Ok []) nodes_array
                  in
                  match nodes_result with
                  | Result.Ok nodes ->
                      Result.Ok (BuildGraph { nodes = List.rev nodes })
                  | Result.Error e -> Result.Error e)
              | None -> Error "nodes field is not an array")
          | None -> Error "Missing nodes field for BuildGraph")
      | Some "WorkspaceConfig" -> (
          match
            ( Json.get_field "workspace_root" json,
              Json.get_field "toolchain" json,
              Json.get_field "packages" json )
          with
          | Some wr, Some tc, Some pkgs -> (
              match
                (Json.get_string wr, Json.get_string tc, Json.get_array pkgs)
              with
              | Some workspace_root, Some toolchain, Some packages_array -> (
                  let packages_result =
                    List.fold_left
                      (fun acc pkg ->
                        match acc with
                        | Result.Error e -> Result.Error e
                        | Result.Ok pkgs_list -> (
                            match Json.get_string pkg with
                            | Some s -> Result.Ok (s :: pkgs_list)
                            | None ->
                                Result.Error "Invalid package in packages array"
                            ))
                      (Result.Ok []) packages_array
                  in
                  match packages_result with
                  | Result.Ok packages ->
                      Result.Ok
                        (WorkspaceConfig
                           {
                             workspace_root;
                             toolchain;
                             packages = List.rev packages;
                           })
                  | Result.Error e -> Result.Error e)
              | _ -> Error "Invalid field types in WorkspaceConfig")
          | _ -> Error "Missing required fields in WorkspaceConfig")
      | Some "Error" -> (
          match Json.get_field "message" json with
          | Some msg_field -> (
              match Json.get_string msg_field with
              | Some msg -> Ok (Error msg)
              | None -> Error "message field is not a string")
          | None -> Error "Missing message field for Error")
      | Some "Success" -> Ok Success
      | Some t -> Error (Printf.sprintf "Unknown response type: %s" t)
      | None -> Error "Type field is not a string")
  | None -> Error "Missing type field in response"
