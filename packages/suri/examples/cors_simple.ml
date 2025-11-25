open Std
open Suri

(** Simple API with CORS enabled for cross-origin requests *)

let routes = Middleware.Router.[
  get "/api/hello" (fun conn _req ->
    conn
    |> Conn.respond ~status:Net.Http.Status.Ok ~body:"Hello from CORS-enabled API!"
    |> Conn.send
  );
  
  get "/api/data" (fun conn _req ->
    let json = {|{"message": "This is accessible from browsers", "status": "ok"}|} in
    conn
    |> Conn.respond ~status:Net.Http.Status.Ok ~body:json
    |> Conn.with_header "content-type" "application/json"
    |> Conn.send
  );
  
  post "/api/submit" (fun conn _req ->
    let body = Conn.body conn in
    Log.info (String.concat "" ["Received POST data: "; body]);
    conn
    |> Conn.respond ~status:Net.Http.Status.Ok ~body:"Data received!"
    |> Conn.send
  );
]

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    (* Development mode - allow all origins *)
    let app = Middleware.[
      request_id;
      logger;
      cors ~origins:["*"] ();
      router routes;
    ] in
    
    let config = Suri.config ~port:4000 () in
    match Suri.start_link ~config app with
    | Ok _supervisor ->
        Log.info "===========================================";
        Log.info "CORS Example Server Running";
        Log.info "===========================================";
        Log.info "Server: http://localhost:4000";
        Log.info "";
        Log.info "Try these endpoints:";
        Log.info "  GET  /api/hello";
        Log.info "  GET  /api/data";
        Log.info "  POST /api/submit";
        Log.info "";
        Log.info "Test with curl:";
        Log.info "  curl -H 'Origin: https://example.com' http://localhost:4000/api/hello";
        Log.info "";
        Log.info "Test preflight:";
        Log.info "  curl -X OPTIONS -H 'Origin: https://example.com' \\";
        Log.info "       -H 'Access-Control-Request-Method: POST' \\";
        Log.info "       http://localhost:4000/api/submit";
        Log.info "===========================================";
        
        (* Keep alive *)
        let rec loop () =
          sleep (Time.Duration.from_secs 100);
          loop ()
        in
        loop ()
    
    | Error `Bind_error ->
        Log.error "Failed to bind to port 4000";
        Error (Failure "Failed to start server")
  )
