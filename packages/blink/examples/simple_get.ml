open Std
open Miniriot

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Log.set_level Log.Debug;

      Log.info "Testing Blink HTTP client...";

      (* Parse URI *)
      let uri = Net.Uri.of_string "http://example.com" |> Result.unwrap in
      Log.info "URI: %s" (Net.Uri.to_string uri);

      (* Connect *)
      Log.info "Connecting...";
      let conn = Blink.connect uri |> Result.unwrap in
      Log.info "Connected!";

      (* Create and send request *)
      let req = Net.Http.Request.create Net.Http.Method.Get uri in
      Log.info "Sending request...";
      let () = Blink.request conn req () |> Result.unwrap in
      Log.info "Request sent!";

      (* Get full response *)
      Log.info "Awaiting response...";
      let response, body = Blink.await conn |> Result.unwrap in

      let status = Net.Http.Response.status response in
      Log.info "Status: %d %s"
        (Net.Http.Status.to_int status)
        (Net.Http.Status.reason_phrase status);
      Log.info "Body length: %d bytes" (String.length body);
      Log.info "Body preview: %s"
        (String.sub body 0 (min 100 (String.length body)));

      Blink.close conn;
      Log.info "Test complete!";
      Ok ())
    ~args:Env.args
