open Std

let ( let* ) value fn = Result.and_then value ~fn

type t = {
  client: Docker_client.Client.t;
  id: string;
  host_name: string;
  host: Net.Addr.stream_addr;
  mutable removed: bool;
}

let address = fun ~host ~port ->
  Net.Addr.from_host_and_port ~host ~port
  |> Result.map_err ~fn:Error.address

let make = fun ~client ~id ~host_name ~host ->
  {
    client;
    id;
    host_name;
    host;
    removed = false;
  }

let id = fun container -> container.id

let host = fun container -> container.host

let logs = fun container ->
  Docker_client.Container.logs container.client ~id:container.id
  |> Result.map_err ~fn:Error.docker

let host_docker_port = fun container ~port ->
  let* inspect =
    Docker_client.Container.inspect container.client ~id:container.id
    |> Result.map_err ~fn:Error.docker
  in
  let rec loop ports =
    match ports with
    | [] -> Error (Error.PortNotExposed port)
    | (container_port, host_port) :: rest ->
        if Docker_client.Port.equal container_port port then
          address ~host:container.host_name ~port:host_port
        else
          loop rest
  in
  loop inspect.Docker_client.Container.ports

let host_port = fun container ~port ->
  host_docker_port
    container
    ~port:(Docker_client.Port.tcp port)

let uri_from_addr = fun ~scheme addr ->
  Net.Uri.Builder.create ()
  |> fun builder ->
    Net.Uri.Builder.scheme builder scheme
    |> fun builder ->
      Net.Uri.Builder.host builder (Net.Addr.ip addr)
      |> fun builder ->
        Net.Uri.Builder.port builder (Net.Addr.port addr)
        |> fun builder ->
          Net.Uri.Builder.path builder "/"
          |> Net.Uri.Builder.build
          |> Result.map_err ~fn:Error.uri

let unique_container_ports = fun ports ->
  let add_port = fun unique port ->
    if List.any unique ~fn:(fun existing -> Docker_client.Port.equal existing port) then
      unique
    else
      port :: unique
  in
  let rec loop unique ports =
    match ports with
    | [] -> List.reverse unique
    | (container_port, _host_port) :: rest -> loop (add_port unique container_port) rest
  in
  loop [] ports

let host_port_for = fun port ports ->
  let rec loop ports =
    match ports with
    | [] -> None
    | (container_port, host_port) :: rest ->
        if Docker_client.Port.equal container_port port then
          Some host_port
        else
          loop rest
  in
  loop ports

let published_port = fun container ->
  let* inspect =
    Docker_client.Container.inspect container.client ~id:container.id
    |> Result.map_err ~fn:Error.docker
  in
  match unique_container_ports inspect.Docker_client.Container.ports with
  | [] -> Error Error.NoPublishedPorts
  | [ container_port ] -> (
      match host_port_for container_port inspect.Docker_client.Container.ports with
      | Some host_port -> address ~host:container.host_name ~port:host_port
      | None -> Error (Error.PortNotExposed container_port)
    )
  | ports -> Error (Error.AmbiguousPublishedPorts ports)

let url = fun ?port ~scheme container ->
  let* addr =
    match port with
    | Some port -> host_port container ~port
    | None -> published_port container
  in
  uri_from_addr ~scheme addr

let remove = fun container ->
  if container.removed then
    Ok ()
  else
    let result =
      Docker_client.Container.remove container.client ~id:container.id
      |> Result.map_err ~fn:Error.docker
    in
    (
      match result with
      | Ok () -> container.removed <- true
      | Error _ -> ()
    );
  result
