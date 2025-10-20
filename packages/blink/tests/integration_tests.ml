open Std
open Miniriot

module BlinkTests = struct
  type Message.t += TestResult of (unit, string) result

  let make_test f () =
    let parent = self () in
    let _test_pid =
      spawn (fun () ->
          yield ();
          yield ();
          yield ();
          let result = f () in
          send parent (TestResult result);
          Ok ())
    in
    let selector msg =
      match msg with TestResult r -> `select r | _ -> `skip
    in
    receive ~selector ()

  let test_get_external () =
    Test.case "Blink: GET request to 127.0.0.1:8080" (fun () ->
        Log.info "Test starting...";
        let uri =
          Net.Uri.of_string "http://127.0.0.1:8080"
          |> Result.expect ~msg:"Invalid URI"
        in
        Log.info "URI parsed, attempting connection...";
        let conn = Blink.connect uri |> Result.expect ~msg:"Blink failed" in
        Log.info "Connected successfully!";
        let req = Net.Http.Request.create Net.Http.Method.Get uri in
        Blink.request conn req ()
        |> Result.expect ~msg:"Request failed"
        |> ignore;
        Log.info "Request sent";
        match Blink.await conn with
        | Error e ->
            Blink.close conn;
            Error
              (format "Await failed: %s"
                 (match e with
                 | `Eof -> "EOF"
                 | `Parse_error msg -> format "Parse: %s" msg
                 | `Protocol_error msg -> format "Protocol: %s" msg
                 | `Closed -> "Connection closed"
                 | `Connection_refused -> "Connection refused"
                 | `System_error msg -> format "System error: %s" msg))
        | Ok (response, _body) ->
            Blink.close conn;
            let status = Net.Http.Response.status response in
            Log.info "Got status: %d" (Net.Http.Status.to_int status);
            if
              Net.Http.Status.to_int status = 200
              || Net.Http.Status.to_int status = 301
            then Ok ()
            else
              Error
                (format "Expected 200 or 301, got %d"
                   (Net.Http.Status.to_int status)))

  let test_post_external () =
    Test.skip "Blink: POST request to httpbin.org" (fun () -> Ok ())

  let test_tcp_loopback () =
    Test.case "TCP loopback test" (fun () ->
        Log.info "Starting TCP loopback test";

        (* Spawn server *)
        let _server =
          spawn (fun () ->
              Log.info "Server: binding";
              let addr =
                Net.Addr.of_host_and_port ~host:"127.0.0.1" ~port:9999
                |> Result.expect ~msg:"Invalid address"
              in
              let listener =
                Net.TcpListener.bind ~reuse_addr:true ~reuse_port:false addr
                |> Result.expect ~msg:"Bind failed"
              in
              Log.info "Server: listening, about to accept";

              let stream, peer =
                Net.TcpListener.accept listener
                |> Result.expect ~msg:"Accept failed"
              in
              Log.info "Server: accepted from %s:%d" (Net.Addr.ip peer)
                (Net.Addr.port peer);

              Net.TcpStream.close stream;
              Net.TcpListener.close listener;
              Ok ())
        in

        (* Give server time *)
        yield ();
        yield ();
        yield ();

        (* Spawn client *)
        let _client =
          spawn (fun () ->
              Log.info "Client: connecting";
              let addr =
                Net.Addr.of_host_and_port ~host:"127.0.0.1" ~port:9999
                |> Result.expect ~msg:"Invalid address"
              in
              let stream =
                Net.TcpStream.connect addr
                |> Result.expect ~msg:"Connect failed"
              in
              Log.info "Client: connected!";
              Net.TcpStream.close stream;
              Ok ())
        in

        yield ();
        yield ();

        Ok ())

  let tests =
    [ test_tcp_loopback (); test_get_external (); test_post_external () ]
end

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Log.info "Main process starting";
      Test.Cli.main ~name:"blink" ~tests:BlinkTests.tests ~args)
    ~args:Env.args
