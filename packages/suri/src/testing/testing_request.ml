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

type error =
  | InvalidUri of {
      value: string;
      reason: Net.Uri.error;
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

let uri_error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Net.Uri.InvalidScheme -> "invalid scheme"
  | Net.Uri.InvalidAuthority -> "invalid authority"
  | Net.Uri.InvalidPath -> "invalid path"
  | Net.Uri.InvalidQuery -> "invalid query"
  | Net.Uri.InvalidFragment -> "invalid fragment"
  | Net.Uri.InvalidFormat -> "invalid format"
  | Net.Uri.TooLong -> "URI is too long"

let error_to_string = fun (InvalidUri { value; reason }) ->
  "invalid testing URI `" ^ value ^ "`: " ^ uri_error_to_string reason

let to_http = fun request ->
  match Net.Uri.from_string request.uri with
  | Error reason -> Error (InvalidUri { value = request.uri; reason })
  | Ok uri ->
      Net.Http.Request.create request.method_ uri
      |> fun http_request ->
        List.fold_left
          request.headers
          ~init:http_request
          ~fn:(fun http_request (name, value) ->
            Net.Http.Request.with_header
              http_request
              name
              value)
        |> fun http_request ->
          if String.equal request.body "" then
            Ok http_request
          else
            Ok (Net.Http.Request.with_body http_request request.body)

let to_web_request = fun request ->
  match to_http request with
  | Error error -> Error error
  | Ok http_request -> Ok (Web_server.Request.from_http ~body:request.body http_request)

let to_conn = fun request ->
  match to_web_request request with
  | Error error -> Error error
  | Ok web_request ->
      Ok (Middleware.Conn.from_request
        ~peer:request.peer
        ~params:request.params
        ~body_params:request.body_params
        web_request)
