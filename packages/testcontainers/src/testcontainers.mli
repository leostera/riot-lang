open Std

type error =
  | Docker of Docker_client.error
  | StartupTimeout of string
  | PortNotExposed of Docker_client.Port.t
  | NoPublishedPorts
  | AmbiguousPublishedPorts of Docker_client.Port.t list
  | AddressError of string
  | UriError of string

val error_to_string: error -> string

(** Return `true` when a local Docker endpoint is configured and appears reachable. *)
val docker_available: unit -> bool

module ReadinessPolicy: sig
  type condition =
    | Running
    | Log of string
    | Healthcheck
    | Delay
  type t = {
    condition: condition;
    duration: Time.Duration.t;
    retry: int;
  }

  val make: duration:Time.Duration.t -> retry:int -> t

  val log: message:string -> duration:Time.Duration.t -> retry:int -> t

  val healthcheck: duration:Time.Duration.t -> retry:int -> t

  val delay: duration:Time.Duration.t -> t

  val condition: t -> condition

  val duration: t -> Time.Duration.t

  val retry: t -> int

  val interval: t -> Time.Duration.t
end

module Generic_image: sig
  module ReadinessPolicy: module type of ReadinessPolicy

  module Duration: sig
    include module type of Time.Duration

    val of_secs: int -> Time.Duration.t

    val of_millis: int -> Time.Duration.t
  end

  type t

  val make: string -> string -> t

  val with_cmd: cmd:string list -> t -> t

  val with_env_var: name:string -> value:string -> t -> t

  val with_label: name:string -> value:string -> t -> t

  val with_exposed_port: port:int -> t -> t

  val with_exposed_docker_port: port:Docker_client.Port.t -> t -> t

  val with_mapped_port: host_port:int -> container_port:int -> t -> t

  val with_mapped_docker_port: host_port:int -> container_port:Docker_client.Port.t -> t -> t

  val with_readiness_policy: policy:ReadinessPolicy.t -> t -> t
end

module Container: sig
  type t

  val id: t -> string

  val host: t -> Net.Addr.stream_addr

  val logs: t -> (string, error) result

  val host_port: t -> port:int -> (Net.Addr.stream_addr, error) result

  val host_docker_port: t -> port:Docker_client.Port.t -> (Net.Addr.stream_addr, error) result

  val url: ?port:int -> scheme:string -> t -> (Net.Uri.t, error) result

  val remove: t -> (unit, error) result
end

val start: Generic_image.t -> (Container.t, error) result

val with_container: Generic_image.t -> (Container.t -> ('a, error) result) -> ('a, error) result
