open Std

type error =
  | ConfigError of string
  | UnsupportedTransport of string
  | UriError of string
  | ConnectError of string
  | HttpError of string
  | DockerError of { status: int; body: string }
  | JsonError of string
  | MissingField of string

val error_to_string: error -> string

module Port: sig
  type protocol =
    | Tcp
    | Udp
    | Sctp
  type t = {
    port: int;
    protocol: protocol;
  }

  val tcp: int -> t

  val udp: int -> t

  val sctp: int -> t

  val to_string: t -> string

  val equal: t -> t -> bool
end

module Config: sig
  type transport =
    | Unix of Path.t
    | Tcp of { host: string; port: int }
  type t = {
    transport: transport;
    platform: string option;
  }

  val make: ?platform:string -> transport -> t

  val from_env: unit -> (t, error) result

  val host_for_containers: t -> string
end

module Client: sig
  type t

  val make: ?config:Config.t -> unit -> (t, error) result

  val config: t -> Config.t

  val ping: t -> (unit, error) result
end

module Image: sig
  val pull: ?platform:string -> Client.t -> name:string -> tag:string -> (unit, error) result
end

module Container: sig
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

  val create_request:
    ?cmd:string list ->
    ?env:(string * string) list ->
    ?labels:(string * string) list ->
    ?exposed_ports:Port.t list ->
    ?port_mappings:port_mapping list ->
    ?publish_all_ports:bool ->
    ?name:string ->
    ?platform:string ->
    image:string ->
    unit ->
    create_request

  val create: Client.t -> create_request -> (string, error) result

  val start: Client.t -> id:string -> (unit, error) result

  val inspect: Client.t -> id:string -> (inspect, error) result

  val logs: Client.t -> id:string -> (string, error) result

  val remove: Client.t -> id:string -> (unit, error) result
end

module Testing: sig
  val container_create_body: Container.create_request -> (string, error) result

  val container_create_path: Container.create_request -> string

  val parse_container_inspect: string -> (Container.inspect, error) result

  val image_create_path:
    ?platform:string ->
    ?config_platform:string ->
    name:string ->
    tag:string ->
    unit ->
    string
end
