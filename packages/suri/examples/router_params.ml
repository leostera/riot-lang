open Std
open Suri

(* Example showing router parameter extraction *)

type article = { id: int; title: string; content: string }

(* Mock database *)

let articles = [
  { id = 1; title = "First Post"; content = "Hello, World!" };
  { id = 2; title = "Second Post"; content = "More content here..." };
  { id = 3; title = "Third Post"; content = "Even more content!" };
]

(* Route handlers that use params *)

let article_handler = fun conn req ->
  let params = Conn.params conn in
  match Std.Collections.Proplist.get params ~key:"id" with
  | Some id_str -> (
      try
        let id = Int.from_string id_str in
        match List.find articles ~fn:(fun a -> a.id = id) with
        | Some article ->
            let json =
              Data.Json.obj
                [
                  ("id", Data.Json.int article.id);
                  ("title", Data.Json.string article.title);
                  ("content", Data.Json.string article.content);
                ]
            in
            conn
            |> Conn.with_status Ok
            |> Conn.with_header "Content-Type" "application/json"
            |> Conn.with_body (Data.Json.to_string json)
            |> Conn.send
        | None ->
            let error = Data.Json.obj [ ("error", Data.Json.string "Article not found"); ] in
            conn
            |> Conn.with_status NotFound
            |> Conn.with_header "Content-Type" "application/json"
            |> Conn.with_body (Data.Json.to_string error)
            |> Conn.send
      with
      | Failure _ ->
          let error = Data.Json.obj [ ("error", Data.Json.string "Invalid article ID"); ] in
          conn
          |> Conn.with_status BadRequest
          |> Conn.with_header "Content-Type" "application/json"
          |> Conn.with_body (Data.Json.to_string error)
          |> Conn.send
    )
  | None ->
      let error = Data.Json.obj [ ("error", Data.Json.string "Missing article ID"); ] in
      conn
      |> Conn.with_status BadRequest
      |> Conn.with_header "Content-Type" "application/json"
      |> Conn.with_body (Data.Json.to_string error)
      |> Conn.send

let articles_list_handler = fun conn req ->
  let articles_json =
    List.map
      articles
      ~fn:(fun a ->
        Data.Json.obj
          [ ("id", Data.Json.int a.id); ("title", Data.Json.string a.title); ])
  in
  let json = Data.Json.array articles_json in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "application/json"
  |> Conn.with_body (Data.Json.to_string json)
  |> Conn.send

let home_handler = fun conn req ->
  let html =
    {|
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
  |}
  in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "text/html"
  |> Conn.with_body html
  |> Conn.send

(* Routes with parameter patterns *)

let routes =
  Middleware.Router.[
    get "/" home_handler;
    get "/articles" articles_list_handler;
    get "/articles/:id" article_handler;
  ]

(* Middleware is just a list! *)

let app = [ Middleware.router routes ]

let main ~args:_ =
  match Suri.start_link app with
  | Ok supervisor ->
      Log.info "🚀 Router params example on http://0.0.0.0:4000";
      Log.info "   Routes:";
      Log.info "     GET  /              - Home page";
      Log.info "     GET  /articles      - List articles";
      Log.info "     GET  /articles/:id  - Get article by ID";
      Log.info "";
      Log.info "   Try:";
      Log.info "     curl http://localhost:4000/articles";
      Log.info "     curl http://localhost:4000/articles/1";
      let count = Supervisor.Dynamic.count_children supervisor in
      Log.info ("   " ^ Int.to_string count.active ^ " acceptors ready");
      let rec loop () =
        sleep (Time.Duration.from_secs 100);
        loop ()
      in
      loop ()
  | Error _ ->
      Log.error "Failed to bind to port 4000";
      Error (Failure "Failed to start server")

let () = Runtime.run ~main ~args:Env.args ()
