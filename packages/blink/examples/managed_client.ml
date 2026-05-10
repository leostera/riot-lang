open Std
open Blink

let main ~args =
  let url =
    Env.var Env.String ~name:"BLINK_EXAMPLE_URL"
    |> Option.unwrap_or ~default:"https://example.com"
  in
  let pool = Client.Config.pool ~max_idle_per_endpoint:4 () in
  let config = Client.Config.make ~connection_policy:(Client.Config.Pool pool) () in
  let client = Client.make ~config () in
  let request = Client.Request.make ~method_:Client.Request.Get ~url () in
  (
    match Client.execute client request with
    | Ok (response, telemetry) ->
        println
          ("status="
          ^ Int.to_string response.status
          ^ " attempts="
          ^ Int.to_string (List.length telemetry.attempts)
          ^ " bytes="
          ^ Int.to_string (String.length response.body))
    | Error error -> println ("request failed: " ^ Client.error_to_string error)
  );
  Client.shutdown client;
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
