open Std
open Miniriot

let () =
  Miniriot.run
    ~main:(fun ~args:_ ->
      Log.set_level Log.Debug;
      Log.info "Testing Blink streaming...";

      match Net.Uri.of_string "http://example.com" with
      | Error _ -> Error (Failure "Failed to parse URI")
      | Ok uri -> (
          match Blink.connect uri with
          | Error _ -> Error (Failure "Failed to connect")
          | Ok conn -> (
              Log.info "Connected to %s" (Net.Uri.to_string uri);

              let req = Net.Http.Request.create Net.Http.Method.Get uri in
              match Blink.request conn req () with
              | Error _ -> Error (Failure "Failed to send request")
              | Ok () -> (
                  Log.info "Request sent, streaming response...";

                  let total_bytes = ref 0 in
                  let rec process_stream () =
                    match Blink.stream conn with
                    | Error e -> Error e
                    | Ok messages ->
                        List.iter
                          (function
                            | `Status status ->
                                Log.info "→ Status: %d %s"
                                  (Net.Http.Status.to_int status)
                                  (Net.Http.Status.reason_phrase status)
                            | `Headers headers ->
                                Log.info "→ Headers: %d"
                                  (Net.Http.Header.length headers);
                                Net.Http.Header.iter
                                  (fun name value ->
                                    Log.debug "  %s: %s" name value)
                                  headers
                            | `Data chunk ->
                                let size = String.length chunk in
                                total_bytes := !total_bytes + size;
                                Log.info "→ Data chunk: %d bytes (total: %d)"
                                  size !total_bytes
                            | `Done ->
                                Log.info "→ Done! Total received: %d bytes"
                                  !total_bytes)
                          messages;

                        if List.mem `Done messages then Ok ()
                        else process_stream ()
                  in

                  match process_stream () with
                  | Error _ -> Error (Failure "Streaming failed")
                  | Ok () ->
                      Blink.close conn;
                      Log.info "Streaming test complete!";
                      Ok ()))))
    ~args:Env.args
