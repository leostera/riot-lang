open Std

let ( let* ) value fn = Result.and_then value ~fn

module GenericImage = Generic_image
module ReadinessPolicy = Readiness_policy

let lift_docker = fun result -> Result.map_err result ~fn:Error.docker

let timeout = fun message -> Error (Error.StartupTimeout message)

let retry_policy = fun policy check ->
  let interval = ReadinessPolicy.interval policy in
  let rec loop remaining =
    if remaining <= 0 then
      timeout "readiness policy did not pass"
    else
      match check () with
      | Ok true -> Ok ()
      | Ok false
      | Error _ ->
          sleep interval;
          loop (remaining - 1)
  in
  loop (ReadinessPolicy.retry policy)

let running = fun container ->
  match Docker_client.Container.inspect container.Container.client ~id:(Container.id container) with
  | Ok inspect -> Ok inspect.Docker_client.Container.state.running
  | Error error -> Error (Error.Docker error)

let logs_contain = fun container message ->
  match Container.logs container with
  | Ok logs -> Ok (String.contains logs message)
  | Error error -> Error error

let healthcheck_passed = fun container ->
  match Docker_client.Container.inspect container.Container.client ~id:(Container.id container) with
  | Error error -> Error (Error.Docker error)
  | Ok inspect -> (
      match inspect.Docker_client.Container.state.health_status with
      | Some "healthy" -> Ok true
      | Some "unhealthy" -> timeout "container reported unhealthy"
      | _ -> Ok false
    )

let wait_one = fun container policy ->
  match ReadinessPolicy.condition policy with
  | ReadinessPolicy.Delay ->
      sleep (ReadinessPolicy.duration policy);
      Ok ()
  | ReadinessPolicy.Running -> retry_policy policy (fun () -> running container)
  | ReadinessPolicy.Log message -> retry_policy policy (fun () -> logs_contain container message)
  | ReadinessPolicy.Healthcheck -> retry_policy policy (fun () -> healthcheck_passed container)

let wait_all = fun container policies ->
  let rec loop policies =
    match policies with
    | [] -> Ok container
    | policy :: rest ->
        let* () = wait_one container policy in
        loop rest
  in
  loop policies

let start = fun image ->
  let* client = lift_docker (Docker_client.Client.make ()) in
  let config = Docker_client.Client.config client in
  let platform = config.Docker_client.Config.platform in
  let* () =
    lift_docker
      (Docker_client.Image.pull ?platform client ~name:image.GenericImage.name ~tag:image.tag)
  in
  let request =
    Docker_client.Container.create_request
      ~cmd:image.cmd
      ~env:image.env
      ~labels:(("org.testcontainers.managed-by", "riot-testcontainers") :: image.labels)
      ~exposed_ports:image.exposed_ports
      ~port_mappings:image.port_mappings
      ~publish_all_ports:(List.is_empty image.port_mappings)
      ?platform
      ~image:(GenericImage.descriptor image)
      ()
  in
  let* id = lift_docker (Docker_client.Container.create client request) in
  let host_name = Docker_client.Config.host_for_containers config in
  let* host = Container.address ~host:host_name ~port:0 in
  let container = Container.make ~client ~id ~host_name ~host in
  let started = lift_docker (Docker_client.Container.start client ~id) in
  let result =
    match started with
    | Error error -> Error error
    | Ok () -> wait_all container image.readiness_policies
  in
  (
    match result with
    | Ok _ -> ()
    | Error _ -> ignore (Container.remove container)
  );
  result

let with_container = fun image fn ->
  let* container = start image in
  let result = fn container in
  let cleanup = Container.remove container in
  match (result, cleanup) with
  | (Ok value, Ok ()) -> Ok value
  | (Error error, _) -> Error error
  | (Ok _, Error error) -> Error error
