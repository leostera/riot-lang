open Std

let ( let* ) value fn = Result.and_then value ~fn

module De = Serde.De
module Ser = Serde.Ser
module Vector = Collections.Vector

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

type create_response_field =
  | CreateResponse_id

type create_response_builder = {
  mutable create_response_id: string option;
}

type health_field =
  | Health_status

type health_builder = {
  mutable health_status: string option;
}

type state_field =
  | State_running
  | State_exit_code
  | State_health

type state_builder = {
  mutable state_running: bool option;
  mutable state_exit_code: int option;
  mutable state_health_status: string option;
}

type inspect_head_field =
  | Inspect_id
  | Inspect_state

type inspect_head_builder = {
  mutable inspect_head_id: string option;
  mutable inspect_head_state: state option;
}

type port_binding_field =
  | PortBinding_host_port

type network_field =
  | Network_ports

type inspect_ports_field =
  | InspectPorts_network

type port_binding_builder = {
  mutable port_binding_host_port: string option;
}

type raw_port_bindings = (string * string option vec option) vec

type network_builder = {
  mutable network_ports: raw_port_bindings option;
}

type inspect_ports_builder = {
  mutable inspect_ports: raw_port_bindings option option option;
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

let serde_error = fun error -> Error.JsonError (Serde.Error.to_string error)

let serde_decode = fun decode body ->
  Serde_json.from_string decode body
  |> Result.map_err ~fn:serde_error

let ser_list = fun encode -> Ser.contramap Vector.from_list (Ser.list encode)

let ser_dict = fun encode -> Ser.contramap Vector.from_list (Ser.dict encode)

let empty_object_encode = Ser.record (Ser.fields [])

let string_map_encode = ser_dict Ser.string

let exposed_ports_encode = fun ports ->
  let ports =
    List.unique
      ports
      ~compare:(fun left right ->
        if Port.equal left right then
          Order.EQ
        else
          String.compare (Port.to_string left) (Port.to_string right))
  in
  Ser.contramap
    (fun () -> List.map ports ~fn:(fun port -> (Port.to_string port, ())))
    (ser_dict empty_object_encode)

let host_port_binding_encode =
  Ser.record (Ser.fields [ Ser.field "HostPort" Ser.string Int.to_string; ])

let port_binding_groups = fun port_mappings ->
  let rec reverse_prepend = fun values tail ->
    match values with
    | [] -> tail
    | value :: rest -> reverse_prepend rest (value :: tail)
  in
  let add_mapping = fun groups mapping ->
    let rec loop acc groups =
      match groups with
      | [] -> List.reverse ((mapping.container_port, [ mapping.host_port ]) :: acc)
      | (port, host_ports) :: rest ->
          if Port.equal port mapping.container_port then
            reverse_prepend acc ((port, mapping.host_port :: host_ports) :: rest)
          else
            loop ((port, host_ports) :: acc) rest
    in
    loop [] groups
  in
  List.fold_left port_mappings ~init:[] ~fn:add_mapping

let port_bindings_encode = fun port_mappings ->
  let groups = port_binding_groups port_mappings in
  Ser.contramap
    (fun () ->
      List.map
        groups
        ~fn:(fun (container_port, host_ports) -> (
          Port.to_string container_port,
          List.reverse host_ports
        )))
    (ser_dict (ser_list host_port_binding_encode))

let host_config_encode = fun request ->
  Ser.record
    (
      Ser.fields
        [
          Ser.field "PublishAllPorts" Ser.bool (fun () -> request.publish_all_ports);
          Ser.field "PortBindings" (port_bindings_encode request.port_mappings) (fun () -> ());
        ]
    )

let create_body_encode = fun request ->
  let fields = [
    Ser.field "Image" Ser.string (fun () -> request.image);
    Ser.field
      "Env"
      (ser_list Ser.string)
      (fun () -> List.map request.env ~fn:(fun (name, value) -> name ^ "=" ^ value));
    Ser.field "Labels" string_map_encode (fun () -> request.labels);
    Ser.field
      "ExposedPorts"
      (exposed_ports_encode
        (request.exposed_ports @ List.map request.port_mappings ~fn:(fun p -> p.container_port)))
      (fun () -> ());
    Ser.field "HostConfig" (host_config_encode request) (fun () -> ());
  ]
  in
  let fields =
    match request.cmd with
    | [] -> fields
    | cmd -> Ser.field "Cmd" (ser_list Ser.string) (fun () -> cmd) :: fields
  in
  Ser.record (Ser.fields fields)

let create_body = fun request ->
  Serde_json.to_string (create_body_encode request) ()
  |> Result.map_err ~fn:serde_error

let create_response_fields = De.fields [ De.field "Id" CreateResponse_id; ]

let create_response_decode =
  De.record_mut
    ~fields:create_response_fields
    ~create:(fun () -> { create_response_id = None })
    ~step:(fun reader builder field ->
      match field with
      | Some CreateResponse_id -> builder.create_response_id <- Some (De.read reader De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> builder.create_response_id)

let parse_create_response = fun body ->
  let* id = serde_decode create_response_decode body in
  match id with
  | Some id -> Ok id
  | None -> Error (Error.MissingField "Id")

let health_fields = De.fields [ De.field "Status" Health_status; ]

let health_decode =
  De.record_mut
    ~fields:health_fields
    ~create:(fun () -> { health_status = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Health_status -> builder.health_status <- De.read reader (De.option De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> builder.health_status)

let state_fields =
  De.fields
    [
      De.field "Running" State_running;
      De.field "ExitCode" State_exit_code;
      De.field "Health" State_health;
    ]

let state_decode =
  De.record_mut
    ~fields:state_fields
    ~create:(fun () -> { state_running = None; state_exit_code = None; state_health_status = None })
    ~step:(fun reader builder field ->
      match field with
      | Some State_running -> builder.state_running <- Some (De.read reader De.bool)
      | Some State_exit_code -> builder.state_exit_code <- De.read reader (De.option De.int)
      | Some State_health ->
          builder.state_health_status <- Option.flatten (De.read reader (De.option health_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> {
      running = Option.unwrap_or builder.state_running ~default:false;
      exit_code = builder.state_exit_code;
      health_status = builder.state_health_status;
    })

let inspect_head_fields = De.fields [ De.field "Id" Inspect_id; De.field "State" Inspect_state; ]

let inspect_head_decode =
  De.record_mut
    ~fields:inspect_head_fields
    ~create:(fun () -> { inspect_head_id = None; inspect_head_state = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Inspect_id -> builder.inspect_head_id <- Some (De.read reader De.string)
      | Some Inspect_state -> builder.inspect_head_state <- Some (De.read reader state_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> builder)

let parse_inspect_head = fun body ->
  let* builder = serde_decode inspect_head_decode body in
  match (builder.inspect_head_id, builder.inspect_head_state) with
  | (Some id, Some state) -> Ok (id, state)
  | (None, _) -> Error (Error.MissingField "Id")
  | (_, None) -> Error (Error.MissingField "State")

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
  let* body = create_body create_request in
  let* body =
    Client.request
      client
      Net.Http.Method.Post
      (create_path create_request)
      ~headers:[ ("content-type", "application/json") ]
      ~body
      ()
  in
  parse_create_response body

let start = fun client ~id ->
  let* _body = Client.request client Net.Http.Method.Post ("/containers/" ^ id ^ "/start") () in
  Ok ()

let vector_to_list = fun values ->
  let items = ref [] in
  Vector.for_each values ~fn:(fun value -> items := value :: !items);
  List.rev !items

let port_binding_fields = De.fields [ De.field "HostPort" PortBinding_host_port; ]

let port_binding_decode =
  De.record_mut
    ~fields:port_binding_fields
    ~create:(fun () -> { port_binding_host_port = None })
    ~step:(fun reader builder field ->
      match field with
      | Some PortBinding_host_port ->
          builder.port_binding_host_port <- De.read reader (De.option De.string)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> builder.port_binding_host_port)

let network_fields = De.fields [ De.field "Ports" Network_ports; ]

let raw_ports_decode = De.dict (De.option (De.list port_binding_decode))

let network_decode =
  De.record_mut
    ~fields:network_fields
    ~create:(fun () -> { network_ports = None })
    ~step:(fun reader builder field ->
      match field with
      | Some Network_ports -> builder.network_ports <- De.read reader (De.option raw_ports_decode)
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> builder.network_ports)

let inspect_ports_fields = De.fields [ De.field "NetworkSettings" InspectPorts_network; ]

let inspect_ports_decode =
  De.record_mut
    ~fields:inspect_ports_fields
    ~create:(fun () -> { inspect_ports = None })
    ~step:(fun reader builder field ->
      match field with
      | Some InspectPorts_network ->
          builder.inspect_ports <- Some (De.read reader (De.option network_decode))
      | None -> ignore (De.read reader De.skip_any))
    ~finish:(fun builder -> builder.inspect_ports)

let parse_host_port = fun port host_port ->
  match host_port with
  | Some host_port -> (
      match Int.parse host_port with
      | Some host_port when host_port > 0 && host_port <= 65_535 -> Ok (Some (port, host_port))
      | Some _
      | None -> Error (Error.JsonError ("invalid Docker host port: " ^ host_port))
    )
  | None -> Ok None

let parse_raw_port_bindings = fun raw_ports ->
  let rec loop_ports acc ports =
    match ports with
    | [] -> Ok (List.reverse acc)
    | (port_text, bindings) :: rest ->
        let* port = Port.of_string port_text in
        (
          match bindings with
          | None -> loop_ports acc rest
          | Some bindings ->
              let rec loop_bindings acc bindings =
                match bindings with
                | [] -> Ok acc
                | host_port :: rest ->
                    let* parsed = parse_host_port port host_port in
                    let acc =
                      match parsed with
                      | None -> acc
                      | Some mapping -> mapping :: acc
                    in
                    loop_bindings acc rest
              in
              let* acc = loop_bindings acc (vector_to_list bindings) in
              loop_ports acc rest
        )
  in
  loop_ports [] (vector_to_list raw_ports)

let parse_port_bindings = fun body ->
  match Serde_json.from_string inspect_ports_decode body with
  | Error error ->
      Error (Error.JsonError ("invalid Docker port binding: " ^ Serde.Error.to_string error))
  | Ok None
  | Ok (Some None)
  | Ok (Some (Some None)) -> Ok []
  | Ok (Some (Some (Some raw_ports))) -> parse_raw_port_bindings raw_ports

let parse_inspect_body = fun body ->
  let* (inspect_id, state) = parse_inspect_head body in
  let* ports = parse_port_bindings body in
  Ok { id = inspect_id; state; ports }

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
