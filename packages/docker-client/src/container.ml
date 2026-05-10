open Std

let ( let* ) value fn = Result.and_then value ~fn

module Json = Data.Json

type port_mapping = {
  host_port: int;
  container_port: Port.t;
}

type create_request = {
  image: string;
  cmd: string list;
  env: (string * string) list;
  labels: (string * string) list;
  exposed_ports: Port.t list;
  port_mappings: port_mapping list;
  publish_all_ports: bool;
  name: string option;
  platform: string option;
}

type state = {
  running: bool;
  exit_code: int option;
  health_status: string option;
}

type inspect = {
  id: string;
  state: state;
  ports: (Port.t * int) list;
}

let create_request = fun
  ?(cmd = [])
  ?(env = [])
  ?(labels = [])
  ?(exposed_ports = [])
  ?(port_mappings = [])
  ?(publish_all_ports = false)
  ?name
  ?platform
  ~image
  () ->
  {
    image;
    cmd;
    env;
    labels;
    exposed_ports;
    port_mappings;
    publish_all_ports;
    name;
    platform;
  }

let empty_object = Json.obj []

let string_list_json = fun values -> Json.array (List.map values ~fn:Json.string)

let string_map_json = fun values ->
  Json.obj
    (List.map values ~fn:(fun (name, value) -> (name, Json.string value)))

let exposed_ports_json = fun ports ->
  Json.obj
    (List.map ports ~fn:(fun port -> (Port.to_string port, empty_object)))

let port_bindings_json = fun port_mappings ->
  Json.obj
    (List.map
      port_mappings
      ~fn:(fun mapping -> (
        Port.to_string mapping.container_port,
        Json.array [ Json.obj [ ("HostPort", Json.string (Int.to_string mapping.host_port)); ]; ]
      )))

let create_body = fun request ->
  let fields = [
    ("Image", Json.string request.image);
    ("Env", string_list_json (List.map request.env ~fn:(fun (name, value) -> name ^ "=" ^ value)));
    ("Labels", string_map_json request.labels);
    (
      "ExposedPorts",
      exposed_ports_json
        (request.exposed_ports @ List.map request.port_mappings ~fn:(fun p -> p.container_port))
    );
    (
      "HostConfig",
      Json.obj
        [
          ("PublishAllPorts", Json.bool request.publish_all_ports);
          ("PortBindings", port_bindings_json request.port_mappings);
        ]
    );
  ]
  in
  let fields =
    match request.cmd with
    | [] -> fields
    | cmd -> ("Cmd", string_list_json cmd) :: fields
  in
  Json.to_string (Json.obj fields)

let create_path = fun request ->
  let params =
    (
      match request.name with
      | None -> []
      | Some name -> [ ("name", name) ]
    ) @ (
      match request.platform with
      | None -> []
      | Some platform -> [ ("platform", platform) ]
    )
  in
  "/containers/create" ^ Api.query params

let create = fun client create_request ->
  let* body =
    Client.request
      client
      Net.Http.Method.Post
      (create_path create_request)
      ~headers:[ ("content-type", "application/json") ]
      ~body:(create_body create_request)
      ()
  in
  let* json = Api.parse_json body in
  Api.json_string_field "Id" json

let start = fun client ~id ->
  let* _body = Client.request client Net.Http.Method.Post ("/containers/" ^ id ^ "/start") () in
  Ok ()

let parse_health_status = fun state_json ->
  match Json.get_field "Health" state_json with
  | None
  | Some Json.Null -> Ok None
  | Some health -> (
      match Json.get_field "Status" health with
      | None
      | Some Json.Null -> Ok None
      | Some value -> (
          match Json.get_string value with
          | Some status -> Ok (Some status)
          | None -> Error (Error.JsonError "State.Health.Status is not a string")
        )
    )

let parse_state = fun json ->
  let* state_json = Api.json_field "State" json in
  let* running = Api.json_bool_field_opt "Running" state_json in
  let* exit_code = Api.json_int_field_opt "ExitCode" state_json in
  let* health_status = parse_health_status state_json in
  Ok { running = Option.unwrap_or running ~default:false; exit_code; health_status }

let parse_host_port = fun port value ->
  match value with
  | Json.Object fields ->
      let host_port =
        match List.find fields ~fn:(fun (name, _) -> String.equal name "HostPort") with
        | Some (_, value) -> Json.get_string value
        | None -> None
      in
      (
        match host_port with
        | Some host_port -> (
            match Int.parse host_port with
            | Some host_port -> Ok (Some (port, host_port))
            | None -> Error (Error.JsonError ("invalid Docker host port: " ^ host_port))
          )
        | None -> Ok None
      )
  | _ -> Ok None

let parse_port_bindings = fun json ->
  match Json.get_field "NetworkSettings" json with
  | None -> Ok []
  | Some network -> (
      match Json.get_field "Ports" network with
      | None
      | Some Json.Null -> Ok []
      | Some ports_json -> (
          match Json.get_object ports_json with
          | None -> Error (Error.JsonError "NetworkSettings.Ports is not an object")
          | Some fields ->
              let rec loop acc fields =
                match fields with
                | [] -> Ok (List.reverse acc)
                | (port_text, bindings) :: rest ->
                    let* port = Port.of_string port_text in
                    (
                      match bindings with
                      | Json.Null -> loop acc rest
                      | Json.Array values ->
                          let rec parse_values acc values =
                            match values with
                            | [] -> Ok acc
                            | value :: values ->
                                let* parsed = parse_host_port port value in
                                let acc =
                                  match parsed with
                                  | None -> acc
                                  | Some mapping -> mapping :: acc
                                in
                                parse_values acc values
                          in
                          let* acc = parse_values acc values in
                          loop acc rest
                      | _ ->
                          Error (Error.JsonError ("invalid Docker port bindings for " ^ port_text))
                    )
              in
              loop [] fields
        )
    )

let parse_inspect_json = fun json ->
  let* inspect_id = Api.json_string_field "Id" json in
  let* state = parse_state json in
  let* ports = parse_port_bindings json in
  Ok { id = inspect_id; state; ports }

let parse_inspect_body = fun body ->
  let* json = Api.parse_json body in
  parse_inspect_json json

let inspect = fun client ~id ->
  let* body = Client.request client Net.Http.Method.Get ("/containers/" ^ id ^ "/json") () in
  parse_inspect_body body

let logs = fun client ~id ->
  Client.request
    client
    Net.Http.Method.Get
    ("/containers/" ^ id ^ "/logs?stdout=1&stderr=1&tail=all")
    ()

let remove = fun client ~id ->
  let* _body =
    Client.request client Net.Http.Method.Delete ("/containers/" ^ id ^ "?force=1&v=1") ()
  in
  Ok ()
