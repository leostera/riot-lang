open Std

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

let current_container: Container.t Std.Test.Context.key = Std.Test.Context.key ()

let setup = fun image context_store ->
  match start image with
  | Error error -> Error (error_to_string error)
  | Ok container ->
      let _ = Std.Test.Context.Store.insert context_store current_container container in
      Ok ()

let teardown = fun context_store ->
  match Std.Test.Context.Store.remove context_store current_container with
  | None -> Ok ()
  | Some container ->
      Container.remove container
      |> Std.Result.map_err ~fn:error_to_string
