open Std

type t = {
  method_: Net.Http.Method.t;
  uri: string;
  headers: (string * string) list;
  body: string;
  peer: Middleware.Conn.peer;
  params: (string * string) list;
  body_params: (string * string) list;
}

let default_peer = Middleware.Conn.{ ip = "127.0.0.1"; port = 0 }

let make = fun
  ?(method_ = Net.Http.Method.Get)
  ?(uri = "/")
  ?(headers = [])
  ?(body = "")
  ?(peer = default_peer)
  ?(params = [])
  ?(body_params = [])
  () ->
  {
    method_;
    uri;
    headers;
    body;
    peer;
    params;
    body_params;
  }

let get = fun ?(headers = []) ?(peer = default_peer) uri ->
  make
    ~method_:Net.Http.Method.Get
    ~uri
    ~headers
    ~peer
    ()

let post = fun ?(headers = []) ?(body = "") ?(peer = default_peer) uri ->
  make
    ~method_:Net.Http.Method.Post
    ~uri
    ~headers
    ~body
    ~peer
    ()

let put = fun ?(headers = []) ?(body = "") ?(peer = default_peer) uri ->
  make
    ~method_:Net.Http.Method.Put
    ~uri
    ~headers
    ~body
    ~peer
    ()

let patch = fun ?(headers = []) ?(body = "") ?(peer = default_peer) uri ->
  make
    ~method_:Net.Http.Method.Patch
    ~uri
    ~headers
    ~body
    ~peer
    ()

let delete = fun ?(headers = []) ?(body = "") ?(peer = default_peer) uri ->
  make
    ~method_:Net.Http.Method.Delete
    ~uri
    ~headers
    ~body
    ~peer
    ()

let to_http = fun request ->
  let uri =
    Net.Uri.of_string request.uri
    |> Result.expect ~msg:("invalid testing URI: " ^ request.uri)
  in
  Net.Http.Request.create request.method_ uri
  |> fun http_request ->
    List.fold_left
      request.headers
      ~init:http_request
      ~fn:(fun http_request ((name, value)) ->
        Net.Http.Request.with_header http_request name value)
    |> fun http_request ->
      if String.equal request.body "" then
        http_request
      else
        Net.Http.Request.with_body http_request request.body

let to_web_request = fun request -> Web_server.Request.of_http ~body:request.body (to_http request)

let to_conn = fun request ->
  to_web_request request
  |> Middleware.Conn.of_request
    ~peer:request.peer
    ~params:request.params
    ~body_params:request.body_params
