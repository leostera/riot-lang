open Std

module Test = Std.Test
module Testcontainers = Testcontainers

let ( let* ) value fn = Result.and_then value ~fn

let test_readiness_policy_clamps_retry = fun _ctx ->
  let policy = Testcontainers.ReadinessPolicy.make ~duration:(Time.Duration.from_secs 3) ~retry:0 in
  if Int.equal (Testcontainers.ReadinessPolicy.retry policy) 1 then
    Ok ()
  else
    Error "expected readiness retry count to be clamped to at least one"

let test_busybox_container_lifecycle = fun _ctx ->
  let marker = "riot-testcontainers-ready" in
  let port = 8_080 in
  let image =
    Testcontainers.Generic_image.(make "busybox" "latest"
    |> with_cmd ~cmd:[ "sh"; "-c"; "echo " ^ marker ^ "; sleep 60" ]
    |> with_exposed_port ~port
    |> with_readiness_policy
      ~policy:(ReadinessPolicy.log ~message:marker ~duration:(Duration.of_secs 10) ~retry:20))
  in
  Testcontainers.with_container
    image
    (fun container ->
      if String.equal (Testcontainers.Container.id container) "" then
        Error (Testcontainers.StartupTimeout "container id was empty")
      else
        let* host_addr = Testcontainers.Container.host_port container ~port in
        if Net.Addr.port host_addr <= 0 then
          Error (Testcontainers.PortNotExposed (Docker_client.Port.tcp port))
        else
          let* uri = Testcontainers.Container.url ~scheme:"test" container in
          if not (Option.equal (Net.Uri.scheme uri) (Some "test") ~fn:String.equal) then
            Error (Testcontainers.StartupTimeout "expected container URL to use requested scheme")
          else if
            not (Option.equal (Net.Uri.port uri) (Some (Net.Addr.port host_addr)) ~fn:Int.equal)
          then
            Error (Testcontainers.StartupTimeout "expected container URL to use published host port")
          else
            let* logs = Testcontainers.Container.logs container in
            if String.contains logs marker then
              Ok ()
            else
              Error (Testcontainers.StartupTimeout "expected container logs to contain startup marker"))
  |> Result.map_err ~fn:Testcontainers.error_to_string

let live_case = fun name fn ->
  if Testcontainers.docker_available () then
    Test.case ~size:Large name fn
  else
    Test.skip ~size:Large name (fun _ctx -> Ok ())

let tests =
  Test.[
    case "readiness policies clamp retry counts" test_readiness_policy_clamps_retry;
    live_case "busybox container lifecycle" test_busybox_container_lifecycle;
  ]

let main ~args =
  Test.Cli.main ~execution_mode:Test.Cli.Linear ~name:"testcontainers_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
