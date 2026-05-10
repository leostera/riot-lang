open Std

let ( let* ) value fn = Result.and_then value ~fn

type transport =
  | Unix of Path.t
  | Tcp of { host: string; port: int }

type t = {
  transport: transport;
  platform: string option;
}

let make = fun ?platform transport -> { transport; platform }

let path_exists = fun path ->
  match Fs.exists path with
  | Ok true -> true
  | Ok false
  | Error _ -> false

let first_existing_socket = fun () ->
  let home = Env.home_dir () in
  let from_home suffix =
    match home with
    | None -> None
    | Some home -> Some Path.(home / Path.v suffix)
  in
  let runtime =
    Env.var Env.String ~name:"XDG_RUNTIME_DIR"
    |> Option.map ~fn:(fun dir -> Path.(Path.v dir / Path.v ".docker/run/docker.sock"))
  in
  let candidates = [
    Some (Path.v "/var/run/docker.sock");
    runtime;
    from_home ".docker/run/docker.sock";
    from_home ".docker/desktop/docker.sock";
  ]
  in
  let rec loop candidates fallback =
    match candidates with
    | [] -> fallback
    | None :: rest -> loop rest fallback
    | Some path :: rest ->
        if path_exists path then
          path
        else
          loop rest fallback
  in
  loop candidates (Path.v "/var/run/docker.sock")

let parse_tcp_host = fun raw ->
  let rest =
    String.sub
      raw
      ~offset:(String.length "tcp://")
      ~len:(String.length raw - String.length "tcp://")
  in
  match String.split_on_char ':' rest with
  | [ host; port ] -> (
      match Int.parse port with
      | Some port -> Ok (Tcp { host; port })
      | None -> Error (Error.ConfigError ("invalid Docker TCP port in " ^ raw))
    )
  | [ host ] -> Ok (Tcp { host; port = 2_375 })
  | _ -> Error (Error.ConfigError ("invalid Docker TCP host in " ^ raw))

let parse_docker_host = fun raw ->
  if String.starts_with ~prefix:"unix://" raw then
    let path =
      String.sub
        raw
        ~offset:(String.length "unix://")
        ~len:(String.length raw - String.length "unix://")
    in
    Ok (Unix (Path.v path))
  else if String.starts_with ~prefix:"tcp://" raw then
    parse_tcp_host raw
  else if
    String.starts_with ~prefix:"http://" raw || String.starts_with ~prefix:"https://" raw
  then
    Error (Error.UnsupportedTransport raw)
  else
    Error (Error.UnsupportedTransport raw)

let from_env = fun () ->
  let platform =
    Env.var Env.String ~name:"DOCKER_DEFAULT_PLATFORM"
    |> Option.and_then
      ~fn:(fun value ->
        let value = String.trim value in
        if String.equal value "" then
          None
        else
          Some value)
  in
  match Env.var Env.String ~name:"DOCKER_HOST" with
  | Some raw when not (String.equal (String.trim raw) "") ->
      let* transport = parse_docker_host raw in
      Ok { transport; platform }
  | _ -> Ok { transport = Unix (first_existing_socket ()); platform }

let host_for_containers = fun config ->
  match config.transport with
  | Unix _ -> "127.0.0.1"
  | Tcp { host; _ } -> host
