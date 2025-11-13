open Std
open Suri

type user = {
  id : int;
  name : string;
  email : string;
}

(* In-memory database *)
let users = [
  { id = 1; name = "Alice"; email = "alice@example.com" };
  { id = 2; name = "Bob"; email = "bob@example.com" };
  { id = 3; name = "Charlie"; email = "charlie@example.com" };
]

let user_to_json user =
  Data.Json.(obj [
    ("id", int user.id);
    ("name", string user.name);
    ("email", string user.email);
  ])

let users_to_json users =
  let user_jsons = List.map user_to_json users in
  Data.Json.array user_jsons

(* CORS middleware *)
let cors_middleware conn =
  conn
  |> Middleware.Conn.with_header "Access-Control-Allow-Origin" "*"

(* Request logger *)
let logger_middleware conn =
  let method_ = Middleware.Conn.method_ conn in
  let uri = Middleware.Conn.uri conn in
  Log.info ((Net.Http.Method.to_string method_) ^ " " ^ uri);
  conn

(* Route handlers *)
let api_info_handler conn =
  let info = Data.Json.obj [
    ("api", Data.Json.string "JSON API Example");
    ("endpoints", Data.Json.obj [
      ("/api/users", Data.Json.string "List all users");
      ("/api/users/:id", Data.Json.string "Get user by ID");
    ])
  ] in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "application/json"
  |> Middleware.Conn.with_body (Data.Json.to_string info)
  |> Middleware.Conn.send

let users_list_handler conn =
  let json = users_to_json users in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "application/json"
  |> Middleware.Conn.with_body (Data.Json.to_string json)
  |> Middleware.Conn.send

let user_handler conn =
  let params = Middleware.Conn.params conn in
  match List.assoc_opt "id" params with
  | Some id_str ->
      (try
        let id = Int.of_string id_str in
        match List.find_opt (fun u -> u.id = id) users with
        | Some user ->
            let json = user_to_json user in
            conn
            |> Middleware.Conn.with_status Ok
            |> Middleware.Conn.with_header "Content-Type" "application/json"
            |> Middleware.Conn.with_body (Data.Json.to_string json)
            |> Middleware.Conn.send
        | None -> 
            let error = Data.Json.obj [("error", Data.Json.string "User not found")] in
            conn
            |> Middleware.Conn.with_status NotFound
            |> Middleware.Conn.with_header "Content-Type" "application/json"
            |> Middleware.Conn.with_body (Data.Json.to_string error)
            |> Middleware.Conn.send
      with Failure _ -> 
        let error = Data.Json.obj [("error", Data.Json.string "Invalid user ID")] in
        conn
        |> Middleware.Conn.with_status BadRequest
        |> Middleware.Conn.with_header "Content-Type" "application/json"
        |> Middleware.Conn.with_body (Data.Json.to_string error)
        |> Middleware.Conn.send)
  | None ->
      let error = Data.Json.obj [("error", Data.Json.string "Missing user ID")] in
      conn
      |> Middleware.Conn.with_status BadRequest
      |> Middleware.Conn.with_header "Content-Type" "application/json"
      |> Middleware.Conn.with_body (Data.Json.to_string error)
      |> Middleware.Conn.send

let not_found_handler conn =
  let error = Data.Json.obj [("error", Data.Json.string "Endpoint not found")] in
  conn
  |> Middleware.Conn.with_status NotFound
  |> Middleware.Conn.with_header "Content-Type" "application/json"
  |> Middleware.Conn.with_body (Data.Json.to_string error)
  |> Middleware.Conn.send

(* Define routes *)
let routes = Middleware.Router.[
  get "/" api_info_handler;
  get "/api/users" users_list_handler;
  get "/api/users/:id" user_handler;
]

(* Build middleware pipeline *)
let app = Middleware.Pipeline.[
  logger_middleware;
  cors_middleware;
  Middleware.Router.middleware routes;
  not_found_handler;
]

(* WebServer handler that runs the middleware pipeline *)
let handler socket_conn req =
  let conn = Middleware.Conn.make socket_conn req in
  let conn = Middleware.Pipeline.run conn app in
  let response = Middleware.Conn.to_response conn in
  WebServer.Handler.close response

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    Log.(set_level Info);
    (* Start the server in its own process *)
    let config = WebServer.Config.make () in
    let supervisor = match WebServer.start_link ~port:3000 ~config ~handler () with
      | Ok s -> s
      | Error `Bind_error ->
          Log.error "Failed to bind to port";
          panic "Failed to start server"
    in
    
    Log.info "JSON API server on http://0.0.0.0:3000";
    Log.info "Try these commands:";
    Log.info "  curl http://localhost:3000/api/users";
    Log.info "  curl http://localhost:3000/api/users/1";
    Log.info "  curl http://localhost:3000/api/users/2";
      
    let count = Supervisor.Dynamic.count_children supervisor in
    Log.info ((Int.to_string count.active) ^ " acceptors ready");
    
    (* Wait forever *)
    let rec loop () =
      sleep (Time.Duration.from_secs 100);
      loop ()
    in
    loop ()
  )
