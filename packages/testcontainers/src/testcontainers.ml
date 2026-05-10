module ReadinessPolicy = Readiness_policy
module Generic_image = Generic_image
module Container = Container

type error = Error.t =
  | Docker of Docker_client.error
  | StartupTimeout of string
  | PortNotExposed of Docker_client.Port.t
  | NoPublishedPorts
  | AmbiguousPublishedPorts of Docker_client.Port.t list
  | AddressError of string
  | UriError of string

let error_to_string = Error.to_string

let docker_available = fun () ->
  match Docker_client.Config.from_env () with
  | Error _ -> false
  | Ok config -> (
      match config.Docker_client.Config.transport with
      | Docker_client.Config.Tcp _ -> true
      | Docker_client.Config.Unix path -> (
          match Std.Fs.exists path with
          | Ok true -> true
          | Ok false
          | Error _ -> false
        )
    )

let start = Lifecycle.start

let with_container = Lifecycle.with_container
