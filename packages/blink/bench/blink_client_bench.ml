open Std

module H = Blink.Client

let sink = ref 0

let keep = fun value -> sink := !sink + value

let request = fun () ->
  H.Request.make
    ~method_:H.Request.Get
    ~url:"https://example.test/data"
    ~deadline:(Time.Duration.from_secs 5)
    ()

let ok_response = H.Response.make ~status:200 ~body:"ok" ()

let bench_request_make = fun () ->
  for _i = 1 to 20_000 do
    keep (String.length (H.Request.describe (request ())))
  done

let bench_status_classification = fun () ->
  for status = 0 to 20_000 do
    let class_ = H.Response.status_class (status mod 700) in
    keep (String.length (H.Response.status_class_to_string class_))
  done

let bench_execute_injected_transport = fun () ->
  let calls = ref 0 in
  let transport _request =
    calls := !calls + 1;
    Ok ok_response
  in
  let budget_policy = H.Budget.policy ~capacity:20_000 ~window:(Time.Duration.from_secs 60) in
  let config = H.Config.make ~budget_policy ~transport () in
  let client = H.make ~config () in
  for _i = 1 to 10_000 do
    match H.execute client (request ()) with
    | Ok (response, telemetry) ->
        keep response.status;
        keep (List.length telemetry.attempts)
    | Error error -> raise (Failure (H.error_to_string error))
  done;
  keep !calls

let hot_path: Bench.bench_config = { iterations = 50; warmup = 5 }

let managed_execute: Bench.bench_config = { iterations = 25; warmup = 5 }

let benchmarks =
  Bench.[
    with_config ~config:hot_path "blink.client request construction" bench_request_make;
    with_config ~config:hot_path "blink.client status classification" bench_status_classification;
    with_config
      ~config:managed_execute
      "blink.client execute with injected transport"
      bench_execute_injected_transport;
  ]

let main ~args = Bench.Cli.main ~name:"blink client benchmarks" ~benchmarks ~args

let () = Runtime.run ~main ~args:Env.args ()
