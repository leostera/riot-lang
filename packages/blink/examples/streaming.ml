open Std
open Miniriot

let () =
  start ~apps:[] @@ fun () ->
  Log.set_level Log.Debug;

  let open Result.Syntax in
  Log.info "Testing Blink streaming...";

  (* Parse URI *)
  let* uri = Net.Uri.of_string "http://example.com" in

  (* Connect *)
  let* conn = Blink.connect uri in
  Log.info "Connected to %s" (Net.Uri.to_string uri);

  (* Create and send request *)
  let req = Net.Http.Request.create Net.Http.Method.Get uri in
  let* () = Blink.request conn req () in
  Log.info "Request sent, streaming response...";

  (* Stream chunks incrementally *)
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
                Log.info "→ Headers: %d" (Net.Http.Header.length headers);
                Net.Http.Header.iter
                  (fun name value -> Log.debug "  %s: %s" name value)
                  headers
            | `Data chunk ->
                let size = String.length chunk in
                total_bytes := !total_bytes + size;
                Log.info "→ Data chunk: %d bytes (total: %d)" size !total_bytes
            | `Done -> Log.info "→ Done! Total received: %d bytes" !total_bytes)
          messages;

        if List.mem `Done messages then Ok () else process_stream ()
  in

  let* () = process_stream () in
  Blink.close conn;
  Log.info "Streaming test complete!";
  Ok ()
