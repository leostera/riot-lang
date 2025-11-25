open Std
open Suri

(** Simple session counter demo *)

let home_handler conn req =
  let session = Middleware.Session.get conn in
  
  (* Get current count from session *)
  let count = match Middleware.Session.get_value "count" session with
    | Option.Some n -> (try int_of_string n with _ -> 0)
    | Option.None -> 0
  in
  
  (* Increment count *)
  let new_count = count + 1 in
  Middleware.Session.put "count" (string_of_int new_count) session;
  
  (* Build response *)
  let html = String.concat "" [
    "<html><head><title>Session Demo</title></head><body>";
    "<h1>Session Counter</h1>";
    "<p>You have visited this page <strong>";
    string_of_int new_count;
    "</strong> times.</p>";
    "<p><a href=\"/reset\">Reset Counter</a></p>";
    "</body></html>"
  ] in
  
  conn
  |> Conn.respond ~status:Ok ~body:html
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let reset_handler conn req =
  let session = Middleware.Session.get conn in
  Middleware.Session.clear session;
  
  let html = String.concat "" [
    "<html><head><title>Session Demo</title></head><body>";
    "<h1>Counter Reset</h1>";
    "<p>Your counter has been reset!</p>";
    "<p><a href=\"/\">Go back</a></p>";
    "</body></html>"
  ] in
  
  conn
  |> Conn.respond ~status:Ok ~body:html
  |> Conn.with_header "content-type" "text/html"
  |> Conn.send

let routes = Middleware.Router.[
  get "/" home_handler;
  get "/reset" reset_handler;
]

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    let secret = "dev-secret-not-for-production-use-32bit" in
    
    let app = Middleware.[
      request_id;
      logger;
      session ~secret ();
      router routes;
    ] in
    
    let config = Suri.config ~port:4000 () in
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
    
    | Error `Bind_error ->
        Log.error "Failed to bind to port 4000";
        Error (Failure "Failed to start server")
  )
