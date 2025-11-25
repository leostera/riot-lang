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
let cors_middleware ~conn ~next =
  let conn' = next conn in
  Conn.with_header "Access-Control-Allow-Origin" "*" conn'

(* Route handlers *)
let api_info_handler conn req =
  let info = Data.Json.obj [
    ("api", Data.Json.string "JSON API Example");
    ("endpoints", Data.Json.obj [
      ("/api/users", Data.Json.string "List all users");
      ("/api/users/:id", Data.Json.string "Get user by ID");
    ])
  ] in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "application/json"
  |> Conn.with_body (Data.Json.to_string info)
  |> Conn.send

let users_list_handler conn req =
  let json = users_to_json users in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "application/json"
  |> Conn.with_body (Data.Json.to_string json)
  |> Conn.send

let user_handler conn req =
  let params = Conn.params conn in
  match List.assoc_opt "id" params with
  | Some id_str ->
      (try
        let id = Int.of_string id_str in
        match List.find_opt (fun u -> u.id = id) users with
        | Some user ->
            let json = user_to_json user in
            conn
            |> Conn.with_status Ok
            |> Conn.with_header "Content-Type" "application/json"
            |> Conn.with_body (Data.Json.to_string json)
            |> Conn.send
        | None -> 
            let error = Data.Json.obj [("error", Data.Json.string "User not found")] in
            conn
            |> Conn.with_status NotFound
            |> Conn.with_header "Content-Type" "application/json"
            |> Conn.with_body (Data.Json.to_string error)
            |> Conn.send
      with Failure _ -> 
        let error = Data.Json.obj [("error", Data.Json.string "Invalid user ID")] in
        conn
        |> Conn.with_status BadRequest
        |> Conn.with_header "Content-Type" "application/json"
        |> Conn.with_body (Data.Json.to_string error)
        |> Conn.send)
  | None ->
      let error = Data.Json.obj [("error", Data.Json.string "Missing user ID")] in
      conn
      |> Conn.with_status BadRequest
      |> Conn.with_header "Content-Type" "application/json"
      |> Conn.with_body (Data.Json.to_string error)
      |> Conn.send

let not_found_handler conn req =
  let error = Data.Json.obj [("error", Data.Json.string "Endpoint not found")] in
  conn
  |> Conn.with_status NotFound
  |> Conn.with_header "Content-Type" "application/json"
  |> Conn.with_body (Data.Json.to_string error)
  |> Conn.send

(* Define routes *)
let routes = Middleware.Router.[
  get "/" api_info_handler;
  get "/api/users" users_list_handler;
  get "/api/users/:id" user_handler;
]

(* App with built-in logger! *)
let app = Middleware.[
  logger;
  cors_middleware;
  router routes;
]

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    Std.Config.load_file (Path.v "packages/suri/examples/conf.toml");
    let _ = Std.Log.start_link () in
    Log.(set_level Info);
    match Suri.start_link app with
    | Ok supervisor ->
        Log.info "🚀 JSON API server on http://0.0.0.0:4000";
        Log.info "   Try these commands:";
        Log.info "     curl http://localhost:4000/api/users";
        Log.info "     curl http://localhost:4000/api/users/1";
        Log.info "     curl http://localhost:4000/api/users/2";
          
        let count = Supervisor.Dynamic.count_children supervisor in
        Log.info ("   " ^ Int.to_string count.active ^ " acceptors ready");
        
        let rec loop () =
          sleep (Time.Duration.from_secs 100);
          loop ()
        in
        loop ()
    | Error `Bind_error ->
        Log.error "Failed to bind to port 4000";
        Error (Failure "Failed to start server")
  )
