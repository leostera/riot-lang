open Std
open Suri

(* Example showing router parameter extraction *)

type article = {
  id : int;
  title : string;
  content : string;
}

(* Mock database *)
let articles = [
  { id = 1; title = "First Post"; content = "Hello, World!" };
  { id = 2; title = "Second Post"; content = "More content here..." };
  { id = 3; title = "Third Post"; content = "Even more content!" };
]

(* Route handlers that use params *)
let article_handler conn =
  let params = Middleware.Conn.params conn in
  match List.assoc_opt "id" params with
  | Some id_str ->
      (try
        let id = Int.of_string id_str in
        match List.find_opt (fun a -> a.id = id) articles with
        | Some article ->
            let json = Data.Json.obj [
              ("id", Data.Json.int article.id);
              ("title", Data.Json.string article.title);
              ("content", Data.Json.string article.content);
            ] in
            conn
            |> Middleware.Conn.with_status Ok
            |> Middleware.Conn.with_header "Content-Type" "application/json"
            |> Middleware.Conn.with_body (Data.Json.to_string json)
            |> Middleware.Conn.send
        | None ->
            let error = Data.Json.obj [("error", Data.Json.string "Article not found")] in
            conn
            |> Middleware.Conn.with_status NotFound
            |> Middleware.Conn.with_header "Content-Type" "application/json"
            |> Middleware.Conn.with_body (Data.Json.to_string error)
            |> Middleware.Conn.send
      with Failure _ ->
        let error = Data.Json.obj [("error", Data.Json.string "Invalid article ID")] in
        conn
        |> Middleware.Conn.with_status BadRequest
        |> Middleware.Conn.with_header "Content-Type" "application/json"
        |> Middleware.Conn.with_body (Data.Json.to_string error)
        |> Middleware.Conn.send)
  | None ->
      let error = Data.Json.obj [("error", Data.Json.string "Missing article ID")] in
      conn
      |> Middleware.Conn.with_status BadRequest
      |> Middleware.Conn.with_header "Content-Type" "application/json"
      |> Middleware.Conn.with_body (Data.Json.to_string error)
      |> Middleware.Conn.send

let articles_list_handler conn =
  let articles_json = List.map (fun a ->
    Data.Json.obj [
      ("id", Data.Json.int a.id);
      ("title", Data.Json.string a.title);
    ]
  ) articles in
  let json = Data.Json.array articles_json in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "application/json"
  |> Middleware.Conn.with_body (Data.Json.to_string json)
  |> Middleware.Conn.send

let home_handler conn =
  let html = {|
<!DOCTYPE html>
<html>
  <head><title>Router Params Example</title></head>
  <body>
    <h1>Router Parameter Extraction</h1>
    <p>Try these URLs:</p>
    <ul>
      <li><a href="/articles">GET /articles</a> - List all articles</li>
      <li><a href="/articles/1">GET /articles/:id</a> - Get article by ID</li>
      <li><a href="/articles/2">GET /articles/2</a></li>
      <li><a href="/articles/999">GET /articles/999</a> - Not found</li>
    </ul>
  </body>
</html>
  |} in
  conn
  |> Middleware.Conn.with_status Ok
  |> Middleware.Conn.with_header "Content-Type" "text/html"
  |> Middleware.Conn.with_body html
  |> Middleware.Conn.send

(* Routes with parameter patterns *)
let routes = Middleware.Router.[
  get "/" home_handler;
  get "/articles" articles_list_handler;
  get "/articles/:id" article_handler;
]

let app = Middleware.Pipeline.[
  Middleware.Router.middleware routes;
]

let handler socket_conn req =
  let conn = Middleware.Conn.make socket_conn req in
  let conn = Middleware.Pipeline.run conn app in
  let response = Middleware.Conn.to_response conn in
  WebServer.Handler.close response

let () =
  Miniriot.run ~args:Env.args () ~main:(fun ~args:_ ->
    (* Start the server in its own process *)
    let _server_pid = spawn (fun () ->
      let config = WebServer.Config.make () in
      let supervisor = match WebServer.start_link ~port:3000 ~config ~handler () with
        | Ok s -> s
        | Error `Bind_error -> panic "Failed to bind to port"
      in
      
      Log.info "Router params example on http://0.0.0.0:3000";
      Log.info "Routes:";
      Log.info "  GET  /              - Home page";
      Log.info "  GET  /articles      - List articles";
      Log.info "  GET  /articles/:id  - Get article by ID";
      Log.info "";
      Log.info "Try:";
      Log.info "  curl http://localhost:3000/articles";
      Log.info "  curl http://localhost:3000/articles/1";
      
      let count = Supervisor.Dynamic.count_children supervisor in
      Log.info ((Int.to_string count.active) ^ " acceptors ready");
      
      (* Server process waits forever *)
      let rec loop () =
        let _ = receive_any () in
        loop ()
      in
      loop ()
    ) in
    
    (* Wait forever *)
    let rec loop () =
      let _ = receive_any () in
      loop ()
    in
    loop ()
  )
