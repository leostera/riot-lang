open Std
open Miniriot

let () =
  Log.set_level Log.Info;

  Miniriot.run
    ~main:(fun ~args:_ ->
      Log.info "Testing Blink HTTP client against httpbin.org...";

      let test_get () =
        Log.info "Test: GET request";
        let uri =
          Net.Uri.of_string "http://httpbin.org/get"
          |> Result.expect ~msg:"Invalid URI"
        in
        let conn =
          Blink.connect uri |> Result.expect ~msg:"Connection failed"
        in
        let req = Net.Http.Request.create Net.Http.Method.Get uri in
        Blink.request conn req ()
        |> Result.expect ~msg:"Request failed"
        |> ignore;
        let response, body =
          Blink.await conn |> Result.expect ~msg:"Await failed"
        in
        Blink.close conn;
        let status = Net.Http.Response.status response in
        Log.info "GET /get returned status %d" (Net.Http.Status.to_int status);
        if Net.Http.Status.to_int status = 200 then
          Log.info "✓ GET request successful"
        else Log.error "✗ GET request failed";
        Ok ()
      in

      let test_post () =
        Log.info "Test: POST request";
        let uri =
          Net.Uri.of_string "http://httpbin.org/post"
          |> Result.expect ~msg:"Invalid URI"
        in
        let conn =
          Blink.connect uri |> Result.expect ~msg:"Connection failed"
        in
        let body_content = "{\"test\":\"data\"}" in
        let headers =
          Net.Http.Header.empty
          |> Net.Http.Header.set "content-type" "application/json"
          |> Net.Http.Header.set "content-length"
               (String.length body_content |> Int.to_string)
        in
        let req =
          Net.Http.Request.create Net.Http.Method.Post uri
          |> Net.Http.Request.with_headers headers
        in
        Blink.request conn req ~body:body_content ()
        |> Result.expect ~msg:"Request failed"
        |> ignore;
        let response, _body =
          Blink.await conn |> Result.expect ~msg:"Await failed"
        in
        Blink.close conn;
        let status = Net.Http.Response.status response in
        Log.info "POST /post returned status %d" (Net.Http.Status.to_int status);
        if Net.Http.Status.to_int status = 200 then
          Log.info "✓ POST request successful"
        else Log.error "✗ POST request failed";
        Ok ()
      in

      let _test1_pid = spawn test_get in
      let _test2_pid = spawn test_post in

      Log.info "All blink tests completed!";
      Ok ())
    ~args:Env.args
  |> exit
