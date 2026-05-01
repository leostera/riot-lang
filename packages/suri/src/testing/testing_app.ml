open Std

module Request = Testing_request

type outcome =
  | Responded of Web_server.Response.t
  | Upgraded

type response_error =
  | InvalidRequest of Request.error
  | ExpectedResponseButUpgraded

let response_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | InvalidRequest error -> Request.error_to_string error
  | ExpectedResponseButUpgraded -> "expected HTTP response, but app upgraded the connection"

let internal_server_error_response = fun () ->
  Web_server.Response.internal_server_error
    ~headers:[ ("content-type", "text/plain; charset=utf-8"); ]
    ~body:"Internal Server Error"
    ()

let conn_to_handler_response = fun conn ->
  match Middleware.Conn.get_upgrade conn with
  | Some upgrade_info -> Web_server.Handler.upgrade upgrade_info.opts upgrade_info.handler
  | None -> Web_server.Handler.respond (Middleware.Conn.to_response conn)

let run_pipeline_response = fun app conn ->
  try
    let conn = Middleware.Pipeline.run conn app in
    conn_to_handler_response conn
  with
  | exn ->
      Log.error
        (String.concat
          ""
          [ "Unhandled exception while handling Suri request: "; Exception.to_string exn; ]);
      Web_server.Handler.respond (internal_server_error_response ())

let run_conn = fun app conn ->
  match run_pipeline_response app conn with
  | Web_server.Handler.Response response -> Responded response
  | Web_server.Handler.Upgrade _ -> Upgraded

let run = fun app request ->
  match Request.to_conn request with
  | Error error -> Error (InvalidRequest error)
  | Ok conn -> Ok (run_conn app conn)

let response = fun app request ->
  match run app request with
  | Error error -> Error error
  | Ok (Responded response) -> Ok response
  | Ok Upgraded -> Error ExpectedResponseButUpgraded

let get = fun app ?headers uri -> response app (Request.get ?headers uri)

let post = fun app ?headers ?body uri -> response app (Request.post ?headers ?body uri)

let put = fun app ?headers ?body uri -> response app (Request.put ?headers ?body uri)

let patch = fun app ?headers ?body uri -> response app (Request.patch ?headers ?body uri)

let delete = fun app ?headers uri -> response app (Request.delete ?headers uri)
