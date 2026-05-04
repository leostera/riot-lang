open Std
open Suri

(** Simple session counter demo *)
let missing_session = fun conn ->
  conn
  |> Conn.respond
    ~status:Net.Http.Status.InternalServerError
    ~body:"Session middleware is not configured"
  |> Conn.send

let home_handler = fun conn _req ->
  match Middleware.Session.get conn with
  | Option.None -> missing_session conn
  | Option.Some session ->
      (* Get current count from session *)
      let count =
        match Middleware.Session.get_value "count" session with
        | Option.Some n ->
            Int.from_string_opt n
            |> Option.unwrap_or ~default:0
        | Option.None -> 0
      in
      (* Increment count *)
      let new_count = count + 1 in
      Middleware.Session.put "count" (string_of_int new_count) session;
      (* Build response *)
      let html =
        String.concat
          ""
          [
            "<html><head><title>Session Demo</title></head><body>";
            "<h1>Session Counter</h1>";
            "<p>You have visited this page <strong>";
            string_of_int new_count;
            "</strong> times.</p>";
            "<p><a href=\"/reset\">Reset Counter</a></p>";
            "</body></html>";
          ]
      in
      conn
      |> Conn.respond ~status:Ok ~body:html
      |> Conn.with_header "content-type" "text/html"
      |> Conn.send

let reset_handler = fun conn _req ->
  match Middleware.Session.get conn with
  | Option.None -> missing_session conn
  | Option.Some session ->
      Middleware.Session.clear session;
      let html =
        String.concat
          ""
          [
            "<html><head><title>Session Demo</title></head><body>";
            "<h1>Counter Reset</h1>";
            "<p>Your counter has been reset!</p>";
            "<p><a href=\"/\">Go back</a></p>";
            "</body></html>";
          ]
      in
      conn
      |> Conn.respond ~status:Ok ~body:html
      |> Conn.with_header "content-type" "text/html"
      |> Conn.send

let routes = Middleware.Router.[ get "/" home_handler; get "/reset" reset_handler ]

let main ~args:_ =
  let secret = "dev-secret-not-for-production-use-32bit" in
  match Middleware.session ~secret () with
  | Error error -> Error (Failure (Middleware.Session.setup_error_to_string error))
  | Ok session_middleware -> (
      let app = Middleware.[ request_id; logger; session_middleware; router routes; ] in
      match Suri.config ~port:4_000 () with
      | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
      | Ok config -> (
          match Suri.start_link ~config app with
          | Ok _supervisor ->
              Log.info "===========================================";
              Log.info "Session Demo Server Running";
              Log.info "===========================================";
              Log.info "Server: http://localhost:4000";
              Log.info "";
              Log.info "Try these endpoints:";
              Log.info "  GET  /       - See visit counter";
              Log.info "  GET  /reset  - Reset counter";
              Log.info "";
              Log.info "Refresh the page to see the counter increment!";
              Log.info "===========================================";
              let rec loop () =
                sleep (Time.Duration.from_secs 100);
                loop ()
              in
              loop ()
          | Error error ->
              Log.error "Failed to bind to port 4000";
              Error (Failure (Suri.start_error_to_string error))
        )
    )

let () = Runtime.run ~main ~args:Env.args ()
