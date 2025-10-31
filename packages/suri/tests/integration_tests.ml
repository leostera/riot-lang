open Std
open Std.Net
open Std.Collections

open Suri

let host = "127.0.0.1"
let port = 9876

module HttpClient = struct
  let make_request method_ port path ~body =
    let uri_string = format "http://%s:%d%s" host port path in
    let uri = Uri.of_string uri_string |> Result.expect ~msg:"Invalid URI" in
    let conn = Blink.connect uri |> Result.expect ~msg:"Connection failed" in
    let headers =
      if body = "" then Http.Header.empty
      else
        Http.Header.set Http.Header.empty "content-length"
          (Int.to_string (String.length body))
    in
    let req_uri = Uri.of_string path |> Result.expect ~msg:"Invalid path" in
    let req = Http.Request.create method_ req_uri in
    let req = Http.Request.with_headers req headers in
    Blink.request conn req ~body ()
    |> Result.expect ~msg:"Request failed"
    |> ignore;
    let response, resp_body =
      Blink.await conn |> Result.expect ~msg:"Receive failed"
    in
    Blink.close conn;
    format "HTTP/1.1 %d %s\r\n\r\n%s"
      (Http.Status.to_int (Http.Response.status response))
      (Http.Status.reason_phrase (Http.Response.status response))
      resp_body

  let get port path = make_request Http.Method.Get port path ~body:""
  let post port path body = make_request Http.Method.Post port path ~body
end

module SuriTestServer = struct
  let handler _conn req =
    let path = WebServer.Request.uri req in
    let method_ = WebServer.Request.method_ req in
    let body = WebServer.Request.body req in

    match (Http.Method.to_string method_, path) with
    | "GET", "/health" -> WebServer.Response.ok ~body:"ok" ()
    | "GET", "/echo_path" -> WebServer.Response.ok ~body:path ()
    | "GET", "/echo_method" ->
        WebServer.Response.ok ~body:(Http.Method.to_string method_) ()
    | "GET", "/status/200" -> WebServer.Response.ok ()
    | "GET", "/status/201" -> WebServer.Response.created ()
    | "GET", "/status/204" -> WebServer.Response.no_content ()
    | "GET", "/status/400" -> WebServer.Response.bad_request ()
    | "GET", "/status/404" -> WebServer.Response.not_found ()
    | "GET", "/status/500" -> WebServer.Response.internal_server_error ()
    | "POST", "/echo_body" -> WebServer.Response.ok ~body ()
    | "GET", "/large_body" ->
        WebServer.Response.ok ~body:(String.make 10000 'a') ()
    | "GET", "/headers" -> (
        let headers = WebServer.Request.headers req in
        match Http.Header.get headers "x-test" with
        | Some value -> WebServer.Response.ok ~body:value ()
        | None -> WebServer.Response.ok ~body:"no-header" ())
    | "GET", "/multiple_headers" ->
        WebServer.Response.ok ~headers:[ ("x-foo", "bar"); ("x-baz", "qux") ] ()
    | _ -> WebServer.Response.not_found ~body:"Not Found" ()

  let server_loop () =
    Log.info "Server process starting...";
    let config = WebServer.Config.make () in
    let handler_state = WebServer.Http1.make_handler ~config ~handler () in
    Log.info "About to start SocketPool...";
    match
      SocketPool.start_link ~host ~port ~acceptors:4
        (module WebServer.Http1)
        handler_state
    with
    | Ok () ->
        Log.info "Socket_pool started successfully";
        Ok ()
    | Error `Bind_error ->
        Log.error "Failed to bind to port %d" port;
        Error (Failure "Failed to bind to port")

  type Message.t += HealthCheckDone

  let await_healthy_loop parent =
    Log.debug "Starting health check loop";
    let rec loop retries =
      if retries <= 0 then (
        Log.error "Server failed health check after max retries";
        send parent HealthCheckDone;
        Ok ())
      else (
        Log.debug "Health check attempt %d" (101 - retries);
        yield ();
        try
          let response = HttpClient.get port "/health" in
          if String.starts_with ~prefix:"HTTP/1.1 200" response then (
            Log.info "Health check succeeded!";
            send parent HealthCheckDone;
            Ok ())
          else (
            Log.debug "Health check returned non-200, retrying";
            loop (retries - 1))
        with exn ->
          Log.debug "Health check exception: %s, retrying"
            (Printexc.to_string exn);
          loop (retries - 1))
    in
    loop 100

  let start () =
    Log.info "Spawning server process...";
    let _server_pid = spawn (fun () -> server_loop ()) in
    Log.info "Spawning health check process...";
    let parent = self () in
    let _health_check_pid = spawn (fun () -> await_healthy_loop parent) in
    Log.info "Waiting for health check...";
    let selector msg =
      match msg with HealthCheckDone -> `select () | _ -> `skip
    in
    receive ~selector ();
    Log.info "Server is healthy and ready";
    port

  let get_port () = port
end

module Html_tests = struct
  open LiveView.Html

  type test_msg = Click | Input of string | Submit

  let test_text_node () =
    Test.case "HTML: text node" (fun () ->
        let html = string "Hello" in
        let output = to_string html in
        if output = "Hello" then Ok ()
        else Error (format "Expected 'Hello', got '%s'" output))

  let test_int_node () =
    Test.case "HTML: int node" (fun () ->
        let html = int 42 in
        let output = to_string html in
        if output = "42" then Ok ()
        else Error (format "Expected '42', got '%s'" output))

  let test_empty_div () =
    Test.case "HTML: empty div" (fun () ->
        let html = div () in
        let output = to_string html in
        if output = "<div ></div>" then Ok ()
        else Error (format "Expected '<div ></div>', got '%s'" output))

  let test_div_with_id () =
    Test.case "HTML: div with id" (fun () ->
        let html = div ~id:"main" () in
        let output = to_string html in
        if output = "<div id=\"main\"></div>" then Ok ()
        else
          Error (format "Expected '<div id=\"main\"></div>', got '%s'" output))

  let test_div_with_attrs () =
    Test.case "HTML: div with multiple attrs" (fun () ->
        let html = div ~id:"main" ~attrs:[ ("class", "container") ] () in
        let output = to_string html in
        if
          output = "<div class=\"container\" id=\"main\"></div>"
          || output = "<div id=\"main\" class=\"container\"></div>"
        then Ok ()
        else
          Error
            (format
               "Expected '<div id=\"main\" class=\"container\"></div>', got \
                '%s'"
               output))

  let test_div_with_children () =
    Test.case "HTML: div with children" (fun () ->
        let html = div ~children:[ string "Hello" ] () in
        let output = to_string html in
        if output = "<div >Hello</div>" then Ok ()
        else Error (format "Expected '<div >Hello</div>', got '%s'" output))

  let test_nested_divs () =
    Test.case "HTML: nested divs" (fun () ->
        let html = div ~children:[ div ~children:[ string "Inner" ] () ] () in
        let output = to_string html in
        if output = "<div ><div >Inner</div></div>" then Ok ()
        else
          Error
            (format "Expected '<div ><div >Inner</div></div>', got '%s'" output))

  let test_h1 () =
    Test.case "HTML: h1 element" (fun () ->
        let html = h1 ~children:[ string "Title" ] () in
        let output = to_string html in
        if output = "<h1 >Title</h1>" then Ok ()
        else Error (format "Expected '<h1 >Title</h1>', got '%s'" output))

  let test_multiple_headings () =
    Test.case "HTML: multiple heading levels" (fun () ->
        let tests =
          [
            (h1 ~children:[ string "H1" ] (), "<h1 >H1</h1>");
            (h2 ~children:[ string "H2" ] (), "<h2 >H2</h2>");
            (h3 ~children:[ string "H3" ] (), "<h3 >H3</h3>");
            (h4 ~children:[ string "H4" ] (), "<h4 >H4</h4>");
            (h5 ~children:[ string "H5" ] (), "<h5 >H5</h5>");
            (h6 ~children:[ string "H6" ] (), "<h6 >H6</h6>");
          ]
        in
        let errors =
          List.filter_map
            (fun (html, expected) ->
              let output = to_string html in
              if output = expected then None
              else Some (format "Expected '%s', got '%s'" expected output))
            tests
        in
        if List.length errors = 0 then Ok ()
        else Error (String.concat "; " errors))

  let test_paragraph () =
    Test.case "HTML: paragraph element" (fun () ->
        let html = p ~children:[ string "Paragraph text" ] () in
        let output = to_string html in
        if output = "<p >Paragraph text</p>" then Ok ()
        else Error (format "Expected '<p >Paragraph text</p>', got '%s'" output))

  let test_span () =
    Test.case "HTML: span element" (fun () ->
        let html = span ~children:[ string "Inline text" ] () in
        let output = to_string html in
        if output = "<span >Inline text</span>" then Ok ()
        else
          Error (format "Expected '<span >Inline text</span>', got '%s'" output))

  let test_button_with_text () =
    Test.case "HTML: button with text" (fun () ->
        let html =
          button
            ~on_click:(on_click (fun _ -> Click))
            ~children:[ string "Click me" ]
            ()
        in
        let output = to_string html in
        if output = "<button >Click me</button>" then Ok ()
        else
          Error
            (format "Expected '<button >Click me</button>', got '%s'" output))

  let test_list_splat () =
    Test.case "HTML: list splat" (fun () ->
        let html = list [ string "A"; string "B"; string "C" ] in
        let output = to_string html in
        if output = "A\nB\nC" then Ok ()
        else Error (format "Expected 'A\\nB\\nC', got '%s'" output))

  let test_complex_tree () =
    Test.case "HTML: complex tree" (fun () ->
        let html =
          div ~id:"app"
            ~children:
              [
                h1 ~children:[ string "Counter" ] ();
                div
                  ~children:
                    [
                      span ~children:[ string "Count: "; int 10 ] ();
                      button
                        ~on_click:(on_click (fun _ -> Click))
                        ~children:[ string "Increment" ]
                        ();
                    ]
                  ();
              ]
            ()
        in
        let output = to_string html in
        let expected =
          "<div id=\"app\"><h1 >Counter</h1>\n\
           <div ><span >Count: \n\
           10</span>\n\
           <button >Increment</button></div></div>"
        in
        if output = expected then Ok ()
        else Error (format "Expected '%s', got '%s'" expected output))

  let test_script_with_src () =
    Test.case "HTML: script with src" (fun () ->
        let html = script ~src:"/app.js" ~type_:"text/javascript" () in
        let output = to_string html in
        let valid_outputs =
          [
            "<script src=\"/app.js\" type=\"text/javascript\"></script>";
            "<script type=\"text/javascript\" src=\"/app.js\"></script>";
          ]
        in
        if List.mem output valid_outputs then Ok ()
        else
          Error
            (format "Expected one of ['%s'], got '%s'"
               (String.concat "', '" valid_outputs)
               output))

  let test_script_with_inline () =
    Test.case "HTML: script with inline code" (fun () ->
        let html = script ~children:[ string "console.log('hi');" ] () in
        let output = to_string html in
        if output = "<script >console.log('hi');</script>" then Ok ()
        else
          Error
            (format "Expected '<script >console.log('hi');</script>', got '%s'"
               output))

  let test_attrs_to_string_empty () =
    Test.case "HTML: attrs_to_string with empty list" (fun () ->
        let output = attrs_to_string [] in
        if output = "" then Ok ()
        else Error (format "Expected empty string, got '%s'" output))

  let test_attrs_to_string_single () =
    Test.case "HTML: attrs_to_string with single attr" (fun () ->
        let output = attrs_to_string [ `attr ("id", "main") ] in
        if output = "id=\"main\"" then Ok ()
        else Error (format "Expected 'id=\"main\"', got '%s'" output))

  let test_attrs_to_string_multiple () =
    Test.case "HTML: attrs_to_string with multiple attrs" (fun () ->
        let output =
          attrs_to_string [ `attr ("id", "main"); `attr ("class", "container") ]
        in
        let valid_outputs =
          [
            "id=\"main\" class=\"container\""; "class=\"container\" id=\"main\"";
          ]
        in
        if List.mem output valid_outputs then Ok ()
        else
          Error
            (format "Expected one of ['%s'], got '%s'"
               (String.concat "', '" valid_outputs)
               output))

  let test_event_handlers_extraction () =
    Test.case "HTML: event handlers extraction" (fun () ->
        let handlers =
          event_handlers
            [
              `attr ("id", "btn");
              `event ("click", fun _ -> Click);
              `event ("input", fun s -> Input s);
            ]
        in
        if List.length handlers = 2 then
          let names = List.map fst handlers in
          if List.mem "click" names && List.mem "input" names then Ok ()
          else
            Error
              (format "Expected ['click', 'input'], got ['%s']"
                 (String.concat "', '" names))
        else Error (format "Expected 2 handlers, got %d" (List.length handlers)))

  let test_on_click_attr () =
    Test.case "HTML: on_click creates correct event attr" (fun () ->
        let handler = on_click (fun _ -> Click) in
        match handler with
        | `event (name, _) ->
            if name = "click" then Ok ()
            else Error (format "Expected event name 'click', got '%s'" name)
        | _ -> Error "Expected event attribute")

  type child_msg = ChildClick
  type parent_msg = ParentClick | ChildMsg of child_msg

  let test_map_action_text () =
    Test.case "HTML: map_action on text node" (fun () ->
        let html = string "Hello" in
        let mapped = map_action (fun m -> ChildMsg m) html in
        let output = to_string mapped in
        if output = "Hello" then Ok ()
        else Error (format "Expected 'Hello', got '%s'" output))

  let test_map_action_element () =
    Test.case "HTML: map_action on element" (fun () ->
        let html = div ~children:[ string "Test" ] () in
        let mapped = map_action (fun m -> ChildMsg m) html in
        let output = to_string mapped in
        if output = "<div >Test</div>" then Ok ()
        else Error (format "Expected '<div >Test</div>', got '%s'" output))

  let test_map_action_with_handler () =
    Test.case "HTML: map_action transforms event handlers" (fun () ->
        let html =
          button
            ~on_click:(on_click (fun _ -> Click))
            ~children:[ string "Click" ]
            ()
        in
        let mapped : parent_msg t = map_action (fun _ -> ParentClick) html in
        let handlers =
          match mapped with El { attrs; _ } -> event_handlers attrs | _ -> []
        in
        if List.length handlers = 1 then
          let _, handler = List.hd handlers in
          let msg = handler "" in
          match msg with
          | ParentClick -> Ok ()
          | _ -> Error "Expected ParentClick message"
        else Error (format "Expected 1 handler, got %d" (List.length handlers)))

  let test_map_action_nested () =
    Test.case "HTML: map_action on nested elements" (fun () ->
        let html =
          div
            ~children:
              [
                button
                  ~on_click:(on_click (fun _ -> Click))
                  ~children:[ string "1" ]
                  ();
                button
                  ~on_click:(on_click (fun _ -> Click))
                  ~children:[ string "2" ]
                  ();
              ]
            ()
        in
        let mapped : parent_msg t = map_action (fun _ -> ParentClick) html in
        let rec count_handlers = function
          | Text _ -> 0
          | Splat els ->
              List.fold_left (fun acc el -> acc + count_handlers el) 0 els
          | El { attrs; children; _ } ->
              List.length (event_handlers attrs)
              + List.fold_left
                  (fun acc el -> acc + count_handlers el)
                  0 children
        in
        let handler_count = count_handlers mapped in
        if handler_count = 2 then Ok ()
        else Error (format "Expected 2 handlers, got %d" handler_count))

  let tests =
    [
      test_text_node ();
      test_int_node ();
      test_empty_div ();
      test_div_with_id ();
      test_div_with_attrs ();
      test_div_with_children ();
      test_nested_divs ();
      test_h1 ();
      test_multiple_headings ();
      test_paragraph ();
      test_span ();
      test_button_with_text ();
      test_list_splat ();
      test_complex_tree ();
      test_script_with_src ();
      test_script_with_inline ();
      test_attrs_to_string_empty ();
      test_attrs_to_string_single ();
      test_attrs_to_string_multiple ();
      test_event_handlers_extraction ();
      test_on_click_attr ();
      test_map_action_text ();
      test_map_action_element ();
      test_map_action_with_handler ();
      test_map_action_nested ();
    ]
end

module Response_tests = struct
  open WebServer.Response

  let test_ok_default () =
    Test.case "Response: ok with defaults" (fun () ->
        let resp = ok () in
        if
          Http.Status.to_int resp.status = 200
          && resp.body = ""
          && Http.Version.to_string resp.version = "HTTP/1.1"
        then Ok ()
        else Error "ok() response incorrect")

  let test_ok_with_body () =
    Test.case "Response: ok with body" (fun () ->
        let resp = ok ~body:"Hello" () in
        if resp.body = "Hello" && Http.Status.to_int resp.status = 200 then
          Ok ()
        else Error "ok() with body incorrect")

  let test_ok_with_headers () =
    Test.case "Response: ok with headers" (fun () ->
        let resp = ok ~headers:[ ("Content-Type", "text/plain") ] () in
        match Http.Header.get resp.headers "content-type" with
        | Some "text/plain" -> Ok ()
        | _ -> Error "ok() with headers incorrect")

  let test_not_found () =
    Test.case "Response: not_found" (fun () ->
        let resp = not_found ~body:"Not Found" () in
        if Http.Status.to_int resp.status = 404 && resp.body = "Not Found" then
          Ok ()
        else Error "not_found() incorrect")

  let test_internal_server_error () =
    Test.case "Response: internal_server_error" (fun () ->
        let resp = internal_server_error () in
        if Http.Status.to_int resp.status = 500 then Ok ()
        else Error "internal_server_error() incorrect")

  let test_created () =
    Test.case "Response: created" (fun () ->
        let resp = created () in
        if Http.Status.to_int resp.status = 201 then Ok ()
        else Error "created() incorrect")

  let test_accepted () =
    Test.case "Response: accepted" (fun () ->
        let resp = accepted () in
        if Http.Status.to_int resp.status = 202 then Ok ()
        else Error "accepted() incorrect")

  let test_no_content () =
    Test.case "Response: no_content" (fun () ->
        let resp = no_content () in
        if Http.Status.to_int resp.status = 204 then Ok ()
        else Error "no_content() incorrect")

  let test_bad_request () =
    Test.case "Response: bad_request" (fun () ->
        let resp = bad_request () in
        if Http.Status.to_int resp.status = 400 then Ok ()
        else Error "bad_request() incorrect")

  let test_unauthorized () =
    Test.case "Response: unauthorized" (fun () ->
        let resp = unauthorized () in
        if Http.Status.to_int resp.status = 401 then Ok ()
        else Error "unauthorized() incorrect")

  let test_forbidden () =
    Test.case "Response: forbidden" (fun () ->
        let resp = forbidden () in
        if Http.Status.to_int resp.status = 403 then Ok ()
        else Error "forbidden() incorrect")

  let test_moved_permanently () =
    Test.case "Response: moved_permanently" (fun () ->
        let resp = moved_permanently () in
        if Http.Status.to_int resp.status = 301 then Ok ()
        else Error "moved_permanently() incorrect")

  let test_found () =
    Test.case "Response: found (302)" (fun () ->
        let resp = found () in
        if Http.Status.to_int resp.status = 302 then Ok ()
        else Error "found() incorrect")

  let test_see_other () =
    Test.case "Response: see_other" (fun () ->
        let resp = see_other () in
        if Http.Status.to_int resp.status = 303 then Ok ()
        else Error "see_other() incorrect")

  let test_not_modified () =
    Test.case "Response: not_modified" (fun () ->
        let resp = not_modified () in
        if Http.Status.to_int resp.status = 304 then Ok ()
        else Error "not_modified() incorrect")

  let test_temporary_redirect () =
    Test.case "Response: temporary_redirect" (fun () ->
        let resp = temporary_redirect () in
        if Http.Status.to_int resp.status = 307 then Ok ()
        else Error "temporary_redirect() incorrect")

  let test_service_unavailable () =
    Test.case "Response: service_unavailable" (fun () ->
        let resp = service_unavailable () in
        if Http.Status.to_int resp.status = 503 then Ok ()
        else Error "service_unavailable() incorrect")

  let tests =
    [
      test_ok_default ();
      test_ok_with_body ();
      test_ok_with_headers ();
      test_not_found ();
      test_internal_server_error ();
      test_created ();
      test_accepted ();
      test_no_content ();
      test_bad_request ();
      test_unauthorized ();
      test_forbidden ();
      test_moved_permanently ();
      test_found ();
      test_see_other ();
      test_not_modified ();
      test_temporary_redirect ();
      test_service_unavailable ();
    ]
end

module Router_tests = struct
  let tests = []
end

module Integration_tests = struct
  type Message.t += TestResult of (unit, string) result

  let make_test_request f () =
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

  let test_get_echo_path () =
    Test.case "Integration: GET /echo_path"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/echo_path" in
           if String.starts_with ~prefix:"HTTP/1.1 200" response then Ok ()
           else Error "Expected 200 response"))

  let test_get_echo_method () =
    Test.case "Integration: GET /echo_method"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/echo_method" in
           if String.starts_with ~prefix:"HTTP/1.1 200" response then Ok ()
           else Error "Expected 200 response"))

  let test_status_200 () =
    Test.case "Integration: status 200"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/status/200" in
           if String.starts_with ~prefix:"HTTP/1.1 200" response then Ok ()
           else Error "Expected 200 OK status"))

  let test_status_201 () =
    Test.case "Integration: status 201"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/status/201" in
           if String.starts_with ~prefix:"HTTP/1.1 201" response then Ok ()
           else Error "Expected 201 status"))

  let test_status_204 () =
    Test.case "Integration: status 204"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/status/204" in
           if String.starts_with ~prefix:"HTTP/1.1 204" response then Ok ()
           else Error "Expected 204 status"))

  let test_status_404 () =
    Test.case "Integration: status 404"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/status/404" in
           if String.starts_with ~prefix:"HTTP/1.1 404" response then Ok ()
           else Error "Expected 404 status"))

  let test_status_500 () =
    Test.case "Integration: status 500"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/status/500" in
           if String.starts_with ~prefix:"HTTP/1.1 500" response then Ok ()
           else Error "Expected 500 status"))

  let test_post_echo_body () =
    Test.case "Integration: POST /echo_body"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.post port "/echo_body" "test body" in
           if String.starts_with ~prefix:"HTTP/1.1 200" response then Ok ()
           else Error "Expected 200 response"))

  let test_large_body () =
    Test.case "Integration: large body response"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/large_body" in
           if String.length response > 10000 then Ok ()
           else
             Error
               (format "Response too small: %d bytes" (String.length response))))

  let test_not_found () =
    Test.case "Integration: 404 for unknown path"
      (make_test_request (fun () ->
           let port = SuriTestServer.get_port () in
           let response = HttpClient.get port "/nonexistent" in
           if String.starts_with ~prefix:"HTTP/1.1 404" response then Ok ()
           else Error "Expected 404 for unknown path"))

  let tests =
    [
      test_get_echo_path ();
      test_get_echo_method ();
      test_status_200 ();
      test_status_201 ();
      test_status_204 ();
      test_status_404 ();
      test_status_500 ();
      test_post_echo_body ();
      test_large_body ();
      test_not_found ();
    ]
end

let () =
  Miniriot.run
    ~main:(fun ~args ->
      Log.(set_level Debug);
      (* let port = SuriTestServer.start () in *)
      Log.info "Starting test server on port %d" port;
      let tests =
        Html_tests.tests @ Response_tests.tests @ Router_tests.tests
        (* @ Integration_tests.tests *)
      in
      match Test.Cli.main ~name:"suri" ~tests ~args with
      | Ok () -> Ok ()
      | Error exn -> Error exn)
    ~args:Env.args;
  exit 0
