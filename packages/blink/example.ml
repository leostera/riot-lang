open Std

let main = fun () ->
  Log.set_level Log.Debug;
  Log.info "Testing Blink HTTP client with IO abstractions";
  match Blink.get "http://example.com" with
  | Error e ->
      Log.error "Request failed: %s"
        (
          match e with
          | `Connection_failed msg -> format "Connection: %s" msg
          | `Read_error msg -> format "Read: %s" msg
          | `Write_error msg -> format "Write: %s" msg
          | `Parse_error msg -> format "Parse: %s" msg
          | `Protocol_error msg -> format "Protocol: %s" msg
        )
  | Ok (response, body) ->
      let status = Net.Http.Response.status response in
      Log.info "Status: %a" Net.Http.Status.pp status;
      Log.info "Body length: %d bytes" (String.length body);
      Log.info "Body preview: %s" (String.sub body 0 (min 200 (String.length body)));
      ()

let () =
  start ~apps:[] @@ fun () -> main ()
