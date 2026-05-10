open Std
open Suri

(** Example showing the debugger middleware in action *)
(* Helper function that will be in the stack trace *)

let find_user = fun id ->
  if id = "123" then
    "Alice"
  else
    panic (String.concat "" [ "User not found: "; id ])

(* Another level in the stack *)

let process_user_request = fun id ->
  let name = find_user id in
  "Hello, " ^ name

(* Route handlers *)

let home_handler = fun conn _req ->
  let html =
    {|
<!DOCTYPE html>
<html>
  <head><title>Debugger Test</title></head>
  <body>
    <h1>Debugger Middleware Test</h1>
    <p>Try these URLs to see the debugger in action:</p>
    <ul>
      <li><a href="/users/123">GET /users/123</a> - Works fine</li>
      <li><a href="/users/999">GET /users/999</a> - Throws exception!</li>
      <li><a href="/crash">GET /crash</a> - Direct crash</li>
      <li><a href="/divide">GET /divide</a> - Division by zero</li>
    </ul>
    <p>The debugger will show:</p>
    <ul>
      <li>🔥 Beautiful error page</li>
      <li>📚 Full stack trace with source code</li>
      <li>📨 Request details</li>
      <li>📤 Response state before error</li>
    </ul>
    <p><strong>Note:</strong> Check your terminal - errors are logged automatically!</p>
  </body>
</html>
  |}
  in
  conn
  |> Conn.with_status Ok
  |> Conn.with_header "Content-Type" "text/html"
  |> Conn.with_body html
  |> Conn.send

let user_handler = fun conn req ->
  let params = Conn.params conn in
  let id =
    Std.Collections.Proplist.get params ~key:"id"
    |> Option.unwrap_or ~default:""
  in
  let result = process_user_request id in
  conn
  |> Conn.respond ~status:Ok ~body:result
  |> Conn.send

let crash_handler = fun conn req ->
  (* Set some response state before crashing *)
  let _conn = Conn.with_header "X-Custom" "value" conn in
  panic "Intentional crash for testing!"

let divide_handler = fun conn req ->
  let x = 10 in
  let y = 0 in
  let result = x / y in
  (* Division by zero! *)
  conn
  |> Conn.respond ~status:Ok ~body:(Int.to_string result)
  |> Conn.send

(* Define routes *)

let routes =
  Middleware.Router.[
    get "/" home_handler;
    get "/users/:id" user_handler;
    get "/crash" crash_handler;
    get "/divide" divide_handler;
  ]

(* App with debugger middleware! *)

let app = Middleware.[ request_id; logger; debugger; router routes; ]

let main ~args:_ =
  (* Enable backtraces! Critical for debugger *)
  Log.(set_level Debug);
  Exception.record_backtrace true;
  match Suri.config ~port:3_000 () with
  | Error errors -> Error (Failure (Suri.Config.errors_to_string errors))
  | Ok config ->
      match Suri.start_link ~config app with
      | Ok supervisor ->
          Log.info
            "╔════════════════════════════════════════════════╗";
          Log.info "║  🐛 Debugger Middleware Test                  ║";
          Log.info "║  http://localhost:3000                        ║";
          Log.info "║                                                ║";
          Log.info "║  Try /users/999 to see beautiful error page!  ║";
          Log.info
            "╚════════════════════════════════════════════════╝";
          Log.info "";
          Log.info "Routes:";
          Log.info "  GET  /           - Home with links";
          Log.info "  GET  /users/:id  - Throws if id != 123";
          Log.info "  GET  /crash      - Direct failwith";
          Log.info "  GET  /divide     - Division by zero";
          Log.info "";
          Log.info "Watch the terminal - errors are logged by debugger!";
          let count = Supervisor.Dynamic.count_children supervisor in
          Log.info (String.concat "" [ Int.to_string count.active; " acceptors ready" ]);
          let rec loop () =
            sleep (Time.Duration.from_secs 100);
            loop ()
          in
          loop ()
      | Error error ->
          Log.error "Failed to bind to port 3000";
          Error (Failure (Suri.start_error_to_string error))

let () = Runtime.run ~main ~args:Env.args ()
