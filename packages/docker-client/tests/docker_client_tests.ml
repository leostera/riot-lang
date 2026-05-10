open Std

module Docker = Docker_client
module De = Serde.De
module Test = Std.Test
module Vector = Collections.Vector

let ( let* ) value fn = Result.and_then value ~fn

let set_env = fun name value ->
  match value with
  | Some value -> Env.set ~var:name ~value
  | None -> Env.remove ~var:name

let restore_env = fun (name, previous) ->
  match previous with
  | Some value -> ignore (Env.set ~var:name ~value)
  | None -> ignore (Env.remove ~var:name)

let with_env = fun updates fn ->
  let previous =
    List.map
      updates
      ~fn:(fun (name, value) ->
        let previous = set_env name value in
        (name, previous))
  in
  let result = fn () in
  List.for_each (List.reverse previous) ~fn:restore_env;
  result

let transport_to_string = fun transport ->
  match transport with
  | Docker.Config.Unix path -> "unix://" ^ Path.to_string path
  | Docker.Config.Tcp { host; port } -> "tcp://" ^ host ^ ":" ^ Int.to_string port

let config_to_string = fun config ->
  let platform =
    match config.Docker.Config.platform with
    | Some platform -> platform
    | None -> "<none>"
  in
  transport_to_string config.Docker.Config.transport ^ " platform=" ^ platform

let test_config_from_env_tcp_with_platform = fun _ctx ->
  with_env
    [
      ("DOCKER_HOST", Some "tcp://docker.example:2376");
      ("DOCKER_DEFAULT_PLATFORM", Some "linux/arm64");
    ]
    (fun () ->
      match Docker.Config.from_env () with
      | Ok config -> (
          match (config.Docker.Config.transport, config.Docker.Config.platform) with
          | (Docker.Config.Tcp { host = "docker.example"; port = 2_376 }, Some "linux/arm64") ->
              Ok ()
          | _ -> Error ("unexpected config: " ^ config_to_string config)
        )
      | Error error -> Error (Docker.error_to_string error))

let test_config_from_env_tcp_default_port = fun _ctx ->
  with_env
    [ ("DOCKER_HOST", Some "tcp://docker.example"); ("DOCKER_DEFAULT_PLATFORM", None); ]
    (fun () ->
      match Docker.Config.from_env () with
      | Ok config -> (
          match (config.Docker.Config.transport, config.Docker.Config.platform) with
          | (Docker.Config.Tcp { host = "docker.example"; port = 2_375 }, None) -> Ok ()
          | _ -> Error ("unexpected config: " ^ config_to_string config)
        )
      | Error error -> Error (Docker.error_to_string error))

let test_config_from_env_trims_docker_host = fun _ctx ->
  with_env
    [ ("DOCKER_HOST", Some "  tcp://docker.example:2375  "); ("DOCKER_DEFAULT_PLATFORM", None); ]
    (fun () ->
      match Docker.Config.from_env () with
      | Ok config -> (
          match config.Docker.Config.transport with
          | Docker.Config.Tcp { host = "docker.example"; port = 2_375 } -> Ok ()
          | _ -> Error ("unexpected config: " ^ config_to_string config)
        )
      | Error error -> Error (Docker.error_to_string error))

let test_config_from_env_rejects_invalid_tcp_port = fun _ctx ->
  with_env
    [ ("DOCKER_HOST", Some "tcp://docker.example:70000"); ("DOCKER_DEFAULT_PLATFORM", None); ]
    (fun () ->
      match Docker.Config.from_env () with
      | Error (Docker.ConfigError message) when String.contains message "invalid Docker TCP port" ->
          Ok ()
      | Error error -> Error ("unexpected error: " ^ Docker.error_to_string error)
      | Ok config -> Error ("expected invalid TCP port, got " ^ config_to_string config))

let test_config_from_env_rejects_empty_unix_socket = fun _ctx ->
  with_env
    [ ("DOCKER_HOST", Some "unix://"); ("DOCKER_DEFAULT_PLATFORM", None); ]
    (fun () ->
      match Docker.Config.from_env () with
      | Error (Docker.ConfigError message) when String.contains message "Unix socket path" -> Ok ()
      | Error error -> Error ("unexpected error: " ^ Docker.error_to_string error)
      | Ok config -> Error ("expected invalid Unix socket path, got " ^ config_to_string config))

let test_config_from_env_unix_socket = fun _ctx ->
  with_env
    [ ("DOCKER_HOST", Some "unix:///tmp/riot-docker.sock"); ("DOCKER_DEFAULT_PLATFORM", None); ]
    (fun () ->
      match Docker.Config.from_env () with
      | Ok config -> (
          match (config.Docker.Config.transport, config.Docker.Config.platform) with
          | (Docker.Config.Unix path, None) when String.equal
            (Path.to_string path)
            "/tmp/riot-docker.sock" -> Ok ()
          | _ -> Error ("unexpected config: " ^ config_to_string config)
        )
      | Error error -> Error (Docker.error_to_string error))

let test_config_from_env_rejects_tls_transport = fun _ctx ->
  with_env
    [ ("DOCKER_HOST", Some "https://docker.example:2376"); ("DOCKER_DEFAULT_PLATFORM", None); ]
    (fun () ->
      match Docker.Config.from_env () with
      | Error (Docker.UnsupportedTransport "https://docker.example:2376") -> Ok ()
      | Error error -> Error ("unexpected error: " ^ Docker.error_to_string error)
      | Ok config -> Error ("expected unsupported transport, got " ^ config_to_string config))

let test_port_rendering_and_equality = fun _ctx ->
  let tcp = Docker.Port.tcp 5_432 in
  let udp = Docker.Port.udp 5_432 in
  if
    String.equal (Docker.Port.to_string tcp) "5432/tcp"
    && String.equal (Docker.Port.to_string udp) "5432/udp"
    && Docker.Port.equal tcp (Docker.Port.tcp 5_432)
    && not (Docker.Port.equal tcp udp)
  then
    Ok ()
  else
    Error "unexpected Docker port rendering or equality behavior"

let test_image_create_path_encodes_platform = fun _ctx ->
  let actual =
    Docker.Testing.image_create_path ~config_platform:"linux/amd64" ~name:"redis" ~tag:"7" ()
  in
  let expected = "/images/create?fromImage=redis&tag=7&platform=linux%2Famd64" in
  if String.equal actual expected then
    Ok ()
  else
    Error ("unexpected image create path: " ^ actual)

let test_image_create_path_explicit_platform_wins = fun _ctx ->
  let actual =
    Docker.Testing.image_create_path
      ~platform:"linux/arm64"
      ~config_platform:"linux/amd64"
      ~name:"redis"
      ~tag:"7"
      ()
  in
  let expected = "/images/create?fromImage=redis&tag=7&platform=linux%2Farm64" in
  if String.equal actual expected then
    Ok ()
  else
    Error ("unexpected image create path: " ^ actual)

type create_body_field =
  | Body_cmd
  | Body_image
  | Body_env
  | Body_labels
  | Body_exposed_ports
  | Body_host_config

type host_config_field =
  | Host_publish_all_ports
  | Host_port_bindings

type host_binding = (string * string) vec

type host_config = {
  publish_all_ports: bool option;
  port_bindings: (string * host_binding vec) vec option;
}

type create_body = {
  cmd: string vec option;
  image: string option;
  env: string vec option;
  labels: (string * string) vec option;
  exposed_ports: (string * unit) vec option;
  host_config: host_config option;
}

type host_config_builder = {
  mutable publish_all_ports: bool option;
  mutable port_bindings: (string * host_binding vec) vec option;
}

type create_body_builder = {
  mutable cmd: string vec option;
  mutable image: string option;
  mutable env: string vec option;
  mutable labels: (string * string) vec option;
  mutable exposed_ports: (string * unit) vec option;
  mutable host_config: host_config option;
}

let vec_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let map_has_key = fun key values ->
  let found = ref false in
  Vector.for_each
    values
    ~fn:(fun (name, _) ->
      if String.equal name key then
        found := true);
  !found

let map_field_count = fun key values ->
  let count = ref 0 in
  Vector.for_each
    values
    ~fn:(fun (name, _) ->
      if String.equal name key then
        count := !count + 1);
  !count

let string_vec_equal = fun actual expected ->
  let actual = vec_to_list actual in
  Int.equal (List.length actual) (List.length expected)
  && List.all (List.zip actual expected) ~fn:(fun (left, right) -> String.equal left right)

let create_body_fields =
  De.fields
    [
      De.field "Cmd" Body_cmd;
      De.field "Image" Body_image;
      De.field "Env" Body_env;
      De.field "Labels" Body_labels;
      De.field "ExposedPorts" Body_exposed_ports;
      De.field "HostConfig" Body_host_config;
    ]

let host_config_fields =
  De.fields
    [
      De.field "PublishAllPorts" Host_publish_all_ports;
      De.field "PortBindings" Host_port_bindings;
    ]

let host_binding_decode = De.map De.string

let host_config_decode =
  De.record_mut
    ~fields:host_config_fields
    ~create:(fun () -> { publish_all_ports = None; port_bindings = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Host_publish_all_ports -> builder.publish_all_ports <- Some (De.read reader De.bool)
      | Some Host_port_bindings ->
          builder.port_bindings <- Some (De.read reader (De.map (De.list host_binding_decode)))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: host_config_builder) ->
      ({ publish_all_ports = builder.publish_all_ports; port_bindings = builder.port_bindings }:
        host_config))

let create_body_decode =
  De.record_mut
    ~fields:create_body_fields
    ~create:(fun () ->
      {
        cmd = None;
        image = None;
        env = None;
        labels = None;
        exposed_ports = None;
        host_config = None;
      })
    ~step:(fun reader builder field ->
      match field with
      | Some Body_cmd -> builder.cmd <- Some (De.read reader (De.list De.string))
      | Some Body_image -> builder.image <- Some (De.read reader De.string)
      | Some Body_env -> builder.env <- Some (De.read reader (De.list De.string))
      | Some Body_labels -> builder.labels <- Some (De.read reader (De.map De.string))
      | Some Body_exposed_ports ->
          builder.exposed_ports <- Some (De.read reader (De.map De.skip_any))
      | Some Body_host_config -> builder.host_config <- Some (De.read reader host_config_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun (builder: create_body_builder) ->
      ({
        cmd = builder.cmd;
        image = builder.image;
        env = builder.env;
        labels = builder.labels;
        exposed_ports = builder.exposed_ports;
        host_config = builder.host_config;
      }: create_body))

let test_container_create_path_encodes_name_and_platform = fun _ctx ->
  let request =
    Docker.Container.create_request ~name:"cache one" ~platform:"linux/amd64" ~image:"redis:7" ()
  in
  let actual = Docker.Testing.container_create_path request in
  let expected = "/containers/create?name=cache+one&platform=linux%2Famd64" in
  if String.equal actual expected then
    Ok ()
  else
    Error ("unexpected container create path: " ^ actual)

let test_container_create_body_is_structured_json = fun _ctx ->
  let request =
    Docker.Container.create_request
      ~cmd:[ "redis-server"; "--save"; "" ]
      ~env:[ ("REDIS_PASSWORD", "secret") ]
      ~labels:[ ("suite", "docker-client") ]
      ~exposed_ports:[ Docker.Port.tcp 6_379 ]
      ~port_mappings:[
        { Docker.Container.host_port = 0; container_port = Docker.Port.tcp 6_379 };
        { Docker.Container.host_port = 6_380; container_port = Docker.Port.tcp 6_379 };
      ]
      ~image:"redis:7"
      ()
  in
  let* body =
    Docker.Testing.container_create_body request
    |> Result.map_err ~fn:Docker.error_to_string
  in
  let* decoded =
    Serde_json.from_string create_body_decode body
    |> Result.map_err ~fn:Serde.Error.to_string
  in
  match (
    decoded.image,
    decoded.cmd,
    decoded.env,
    decoded.labels,
    decoded.exposed_ports,
    decoded.host_config
  ) with
  | (Some "redis:7", Some cmd, Some env, Some labels, Some exposed, Some host_config) when string_vec_equal
    cmd
    [ "redis-server"; "--save"; "" ]
  && string_vec_equal env [ "REDIS_PASSWORD=secret" ]
  && map_has_key "suite" labels
  && map_has_key "6379/tcp" exposed -> (
      match host_config.port_bindings with
      | Some port_bindings when Bool.equal
        (Option.unwrap_or host_config.publish_all_ports ~default:true)
        false
      && map_has_key "6379/tcp" port_bindings ->
          if not (Int.equal (map_field_count "6379/tcp" exposed) 1) then
            Error "expected duplicate exposed Docker ports to be collapsed"
          else if not (Int.equal (map_field_count "6379/tcp" port_bindings) 1) then
            Error "expected duplicate Docker port bindings to be grouped"
          else
            let bindings =
              vec_to_list port_bindings
              |> List.find ~fn:(fun (name, _) -> String.equal name "6379/tcp")
            in
            (
              match bindings with
              | Some (_, bindings) when Int.equal (Vector.len bindings) 2 -> Ok ()
              | Some _ -> Error "expected grouped Docker port binding to keep both host ports"
              | None -> Error "expected Docker port binding to include 6379/tcp"
            )
      | _ -> Error "expected Docker host config to include grouped port bindings"
    )
  | _ -> Error "unexpected Docker create body"

let has_port_mapping = fun mappings port host_port ->
  List.any
    mappings
    ~fn:(fun (container_port, mapped_host_port) ->
      Docker.Port.equal container_port port && Int.equal mapped_host_port host_port)

let test_parse_container_inspect_extracts_state_and_ports = fun _ctx ->
  let source =
    {|{
  "Id": "abc123",
  "State": {
    "Running": true,
    "ExitCode": 0,
    "Health": { "Status": "healthy" }
  },
  "NetworkSettings": {
    "Ports": {
      "6379/tcp": [{ "HostIp": "0.0.0.0", "HostPort": "49153" }],
      "8080/udp": null
    }
  }
}|}
  in
  match Docker.Testing.parse_container_inspect source with
  | Ok inspect when String.equal inspect.Docker.Container.id "abc123"
  && inspect.Docker.Container.state.Docker.Container.running
  && Option.equal inspect.Docker.Container.state.Docker.Container.exit_code (Some 0) ~fn:Int.equal
  && Option.equal
    inspect.Docker.Container.state.Docker.Container.health_status
    (Some "healthy")
    ~fn:String.equal
  && has_port_mapping inspect.Docker.Container.ports (Docker.Port.tcp 6_379) 49_153 -> Ok ()
  | Ok _ -> Error "unexpected parsed container inspect result"
  | Error error -> Error (Docker.error_to_string error)

let test_parse_container_inspect_rejects_invalid_host_port = fun _ctx ->
  let source =
    {|{
  "Id": "abc123",
  "State": { "Running": false, "ExitCode": 1 },
  "NetworkSettings": {
    "Ports": {
      "6379/tcp": [{ "HostPort": "not-a-port" }]
    }
  }
}|}
  in
  match Docker.Testing.parse_container_inspect source with
  | Error (Docker.JsonError message) when String.contains message "invalid Docker host port" ->
      Ok ()
  | Error error -> Error ("unexpected error: " ^ Docker.error_to_string error)
  | Ok _ -> Error "expected invalid host port to be rejected"

let test_parse_container_inspect_rejects_out_of_range_host_port = fun _ctx ->
  let source =
    {|{
  "Id": "abc123",
  "State": { "Running": false, "ExitCode": 1 },
  "NetworkSettings": {
    "Ports": {
      "6379/tcp": [{ "HostPort": "70000" }]
    }
  }
}|}
  in
  match Docker.Testing.parse_container_inspect source with
  | Error (Docker.JsonError message) when String.contains message "invalid Docker host port" ->
      Ok ()
  | Error error -> Error ("unexpected error: " ^ Docker.error_to_string error)
  | Ok _ -> Error "expected out-of-range host port to be rejected"

let test_parse_container_inspect_rejects_invalid_binding_shape = fun _ctx ->
  let source =
    {|{
  "Id": "abc123",
  "State": { "Running": false, "ExitCode": 1 },
  "NetworkSettings": {
    "Ports": {
      "6379/tcp": ["not-an-object"]
    }
  }
}|}
  in
  match Docker.Testing.parse_container_inspect source with
  | Error (Docker.JsonError message) when String.contains message "invalid Docker port binding" ->
      Ok ()
  | Error error -> Error ("unexpected error: " ^ Docker.error_to_string error)
  | Ok _ -> Error "expected malformed port binding to be rejected"

let docker_result = fun result -> Result.map_err result ~fn:Docker.error_to_string

let with_created_container = fun client request fn ->
  match Docker.Container.create client request with
  | Error error -> Error (Docker.error_to_string error)
  | Ok id ->
      let result = fn id in
      let cleanup_result = Docker.Container.remove client ~id in
      (
        match (result, cleanup_result) with
        | (Ok (), Ok ()) -> Ok ()
        | (Error message, _) -> Error message
        | (Ok (), Error error) ->
            Error ("failed to clean up Docker container: " ^ Docker.error_to_string error)
      )

let wait_for_logs = fun client ~id ~message ->
  let rec loop attempts =
    if attempts <= 0 then
      Error ("timed out waiting for Docker logs to contain " ^ message)
    else
      match Docker.Container.logs client ~id with
      | Ok logs when String.contains logs message -> Ok ()
      | Ok _ ->
          sleep (Time.Duration.from_millis 100);
          loop (attempts - 1)
      | Error error -> Error (Docker.error_to_string error)
  in
  loop 50

let test_live_docker_ping = fun _ctx ->
  let* client =
    Docker.Client.make ()
    |> docker_result
  in
  Docker.Client.ping client
  |> docker_result

let test_live_container_lifecycle = fun _ctx ->
  let marker = "riot-docker-client-ready" in
  let port = Docker.Port.tcp 8_080 in
  let* client =
    Docker.Client.make ()
    |> docker_result
  in
  let* () =
    Docker.Client.ping client
    |> docker_result
  in
  let* () =
    Docker.Image.pull client ~name:"busybox" ~tag:"latest"
    |> docker_result
  in
  let request =
    Docker.Container.create_request
      ~cmd:[ "sh"; "-c"; "echo " ^ marker ^ "; sleep 60" ]
      ~labels:[ ("riot.test", "docker-client") ]
      ~exposed_ports:[ port ]
      ~publish_all_ports:true
      ~image:"busybox:latest"
      ()
  in
  with_created_container
    client
    request
    (fun id ->
      let* () =
        Docker.Container.start client ~id
        |> docker_result
      in
      let* inspect =
        Docker.Container.inspect client ~id
        |> docker_result
      in
      if not inspect.Docker.Container.state.Docker.Container.running then
        Error "expected Docker container to be running after start"
      else if
        not
          (List.any
            inspect.Docker.Container.ports
            ~fn:(fun (published, host_port) -> Docker.Port.equal published port && host_port > 0))
      then
        Error "expected Docker to publish the requested container port"
      else
        wait_for_logs client ~id ~message:marker)

let docker_endpoint_available = fun () ->
  match Docker.Config.from_env () with
  | Error _ -> false
  | Ok config -> (
      match config.Docker.Config.transport with
      | Docker.Config.Tcp _ -> true
      | Docker.Config.Unix path -> (
          match Fs.exists path with
          | Ok true -> true
          | Ok false
          | Error _ -> false
        )
    )

let live_case = fun name fn ->
  if docker_endpoint_available () then
    Test.case ~size:Large name fn
  else
    Test.skip ~size:Large name (fun _ctx -> Ok ())

let tests =
  Test.[
    case "Config.from_env parses TCP host and platform" test_config_from_env_tcp_with_platform;
    case "Config.from_env applies Docker TCP default port" test_config_from_env_tcp_default_port;
    case "Config.from_env trims Docker host" test_config_from_env_trims_docker_host;
    case "Config.from_env rejects invalid TCP ports" test_config_from_env_rejects_invalid_tcp_port;
    case
      "Config.from_env rejects empty Unix socket paths"
      test_config_from_env_rejects_empty_unix_socket;
    case "Config.from_env parses Unix socket host" test_config_from_env_unix_socket;
    case "Config.from_env rejects TLS transport" test_config_from_env_rejects_tls_transport;
    case "Port rendering and equality are deterministic" test_port_rendering_and_equality;
    case "Image create path encodes configured platform" test_image_create_path_encodes_platform;
    case
      "Image create path lets explicit platform override config"
      test_image_create_path_explicit_platform_wins;
    case
      "Container create path encodes name and platform"
      test_container_create_path_encodes_name_and_platform;
    case "Container create body is structured JSON" test_container_create_body_is_structured_json;
    case
      "Container inspect parsing extracts state and published ports"
      test_parse_container_inspect_extracts_state_and_ports;
    case
      "Container inspect parsing rejects invalid host ports"
      test_parse_container_inspect_rejects_invalid_host_port;
    case
      "Container inspect parsing rejects out-of-range host ports"
      test_parse_container_inspect_rejects_out_of_range_host_port;
    case
      "Container inspect parsing rejects malformed port bindings"
      test_parse_container_inspect_rejects_invalid_binding_shape;
    live_case "live Docker ping" test_live_docker_ping;
    live_case "live Docker container lifecycle" test_live_container_lifecycle;
  ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"docker_client_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
