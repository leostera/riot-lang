open Std
open Suri

(** Content negotiation example - JSON-only API *)
let routes =
  Middleware.Router.[
    get
      "/"
      (fun conn _req ->
        let html =
          String.concat
            ""
            [
              "<html><head><title>Content Negotiation Demo</title></head><body>";
              "<h1>Content Negotiation Example</h1>";
              "<p>This API only accepts JSON requests.</p>";
              "<h2>Try these endpoints:</h2>";
              "<ul>";
              "<li><code>GET /api/data</code> - Returns JSON data</li>";
              "<li><code>POST /api/submit</code> - Accepts JSON only</li>";
              "</ul>";
              "<h3>Test commands:</h3>";
              "<pre>";
              "# This works (Accept: application/json)\n";
              "curl -H 'Accept: application/json' http://localhost:3001/api/data\n\n";
              "# This fails (Accept: text/html)\n";
              "curl -H 'Accept: text/html' http://localhost:3001/api/data\n\n";
              "# This works (Content-Type: application/json)\n";
              "curl -X POST -H 'Content-Type: application/json' \\\n";
              "     -d '{\"key\":\"value\"}' http://localhost:3001/api/submit\n\n";
              "# This fails (Content-Type: text/plain)\n";
              "curl -X POST -H 'Content-Type: text/plain' \\\n";
              "     -d 'plain text' http://localhost:3001/api/submit";
              "</pre>";
              "</body></html>";
            ]
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:html
        |> Conn.with_header "content-type" "text/html"
        |> Conn.send);
    get
      "/api/data"
      (fun conn _req ->
        let json = {|{"message": "JSON data", "items": [1, 2, 3], "status": "ok"}|} in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
        |> Conn.with_header "content-type" "application/json"
        |> Conn.send);
    post
      "/api/submit"
      (fun conn _req ->
        let body = Conn.body conn in
        let response =
          String.concat
            ""
            [
              "{\"message\": \"Data received\", \"received_bytes\": ";
              string_of_int (String.length body);
              "}";
            ]
        in
        conn
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:response
        |> Conn.with_header "content-type" "application/json"
        |> Conn.send);
  ]

let main ~args:_ =
  let app =
    Middleware.[
      request_id;
      logger;
      accepts
        ~config:Middleware.Accepts.{
          types = [ "application/json" ];
          check_accept = true;
          check_content_type = true;
          on_reject = None;
        }
        [];
      router routes;
    ]
  in
  match Suri.config ~port:3_001 () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      match Suri.start_link ~config app with
      | Ok _supervisor ->
          Log.info "===========================================";
          Log.info "Content Negotiation Example Server Running";
          Log.info "===========================================";
          Log.info "Server: http://localhost:3001";
          Log.info "";
          Log.info "This API only accepts application/json";
          Log.info "";
          Log.info "Routes:";
          Log.info "  GET  /          - Info page (HTML allowed)";
          Log.info "  GET  /api/data  - JSON data (JSON only)";
          Log.info "  POST /api/submit - Submit data (JSON only)";
          Log.info "";
          Log.info "Test Accept header:";
          Log.info "  curl -H 'Accept: application/json' http://localhost:3001/api/data";
          Log.info "  curl -H 'Accept: text/html' http://localhost:3001/api/data  # 406";
          Log.info "";
          Log.info "Test Content-Type header:";
          Log.info "  curl -X POST -H 'Content-Type: application/json' \\";
          Log.info "       -d '{\"test\":\"data\"}' http://localhost:3001/api/submit";
          Log.info "  curl -X POST -H 'Content-Type: text/plain' \\";
          Log.info "       -d 'text' http://localhost:3001/api/submit  # 415";
          Log.info "===========================================";
          (* Keep alive *)
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error error ->
          Log.error "Failed to bind to port 3001";
          Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
