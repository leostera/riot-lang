open Std
open Std.Collections

type peer = { ip: string; port: int }

type upgrade_info = {
  opts: Channel.Handler.upgrade_opts;
  handler: Channel.Handler.t;
}

type 'a assign_key = 'a TypedKeyHashMap.key

type t = {
  socket_conn: Socket_pool.Connection.t option;
  req: Web_server.Request.t;
  peer: peer;
  method_override: Net.Http.Method.t option;
  (* For method override middleware *)
  params: (string * string) list;
  body_params: (string * string) list;
  resp_status: Net.Http.Status.t;
  resp_headers: (string * string) list;
  resp_body: string;
  halted: bool;
  sent: bool;
  upgrade: upgrade_info option;
  assigns: TypedKeyHashMap.t;
}

let make_from_request = fun ?socket_conn ~req ~peer ?(params = []) ?(body_params = []) () ->
  {
    socket_conn;
    req;
    peer;
    method_override = None;
    params;
    body_params;
    resp_status = Ok;
    resp_headers = [];
    resp_body = "";
    halted = false;
    sent = false;
    upgrade = None;
    assigns = TypedKeyHashMap.create ();
  }

let make = fun socket_conn req ->
  let peer_addr = Socket_pool.Connection.peer socket_conn in
  let peer = { ip = Net.Addr.ip peer_addr; port = Net.Addr.port peer_addr } in
  make_from_request ~socket_conn ~req ~peer ()

let from_request = fun
  ?(peer = {ip = "127.0.0.1"; port = 0}) ?(params = []) ?(body_params = []) req ->
  make_from_request
    ~req
    ~peer
    ~params
    ~body_params
    ()

let request = fun t -> t.req

let method_ = fun t ->
  match t.method_override with
  | Some m -> m
  | None -> Web_server.Request.method_ t.req

let uri = fun t -> Web_server.Request.uri t.req

let path = fun t ->
  let uri_str = Web_server.Request.uri t.req in
  match String.index_of uri_str ~char:'?' with
  | Some idx -> String.sub uri_str ~offset:0 ~len:idx
  | None -> uri_str

let headers = fun t -> Web_server.Request.headers t.req

let with_request_header = fun name value t -> {
  t with
  req = Web_server.Request.with_header name value t.req;
}

let body = fun t -> Web_server.Request.body t.req

let params = fun t -> t.params

let parse_query_params = fun query_string ->
  query_string
  |> String.split_on_char '&'
  |> List.filter ~fn:(fun part -> not (String.equal part ""))
  |> String.concat "&"
  |> Net.Uri.Query.parse

let query_params = fun t ->
  let uri_str = Web_server.Request.uri t.req in
  match String.index_of uri_str ~char:'?' with
  | None -> []
  | Some idx ->
      let query_string =
        String.sub uri_str ~offset:(idx + 1) ~len:(String.length uri_str - idx - 1)
      in
      parse_query_params query_string

let body_params = fun t -> t.body_params

let peer = fun t -> t.peer

let resp_headers = fun t -> t.resp_headers

let with_status = fun status t -> { t with resp_status = status }

let with_body = fun body t -> { t with resp_body = body }

let with_header = fun name value t ->
  {
    t with
    resp_headers = (name, value) :: t.resp_headers;
  }

let header_name_equal = fun left right ->
  String.equal
    (String.lowercase_ascii left)
    (String.lowercase_ascii right)

let set_header = fun name value t ->
  {
    t with
    resp_headers = (name, value)
    :: List.filter
      t.resp_headers
      ~fn:(fun (existing, _value) -> not (header_name_equal existing name));
  }

let with_method = fun method_ t -> { t with method_override = Some method_ }

let with_peer = fun peer t -> { t with peer }

let respond = fun ~status ?body t ->
  let t = with_status status t in
  match body with
  | Some b -> with_body b t
  | None -> t

let send = fun t -> { t with sent = true }

let sent = fun t -> t.sent

let render_component = fun ?(headers = []) status component t ->
  let t =
    List.fold_left headers ~init:t ~fn:(fun acc (name, value) -> with_header name value acc)
  in
  t
  |> with_status status
  |> with_header "Content-Type" "text/html; charset=utf-8"
  |> with_body (Component.to_html component)
  |> send

let render_json = fun ?(headers = []) status json t ->
  let t =
    List.fold_left headers ~init:t ~fn:(fun acc (name, value) -> with_header name value acc)
  in
  t
  |> with_status status
  |> with_header "Content-Type" "application/json"
  |> with_body (Data.Json.to_string json)
  |> send

let render_text = fun ?(headers = []) status text t ->
  let t =
    List.fold_left headers ~init:t ~fn:(fun acc (name, value) -> with_header name value acc)
  in
  t
  |> with_status status
  |> with_header "Content-Type" "text/plain; charset=utf-8"
  |> with_body text
  |> send

let redirect = fun ?(headers = []) path t ->
  let t =
    List.fold_left headers ~init:t ~fn:(fun acc (name, value) -> with_header name value acc)
  in
  t
  |> with_status Found
  |> with_header "Location" path
  |> with_body ""
  |> send

let halt = fun t -> { t with halted = true }

let halted = fun t -> t.halted

let set_params = fun params t -> { t with params }

let set_body_params = fun body_params t -> { t with body_params }

let socket_conn = fun t -> t.socket_conn

let upgrade_websocket = fun opts handler t -> {
  t with
  upgrade = Some { opts; handler };
  halted = true;
}

let get_upgrade = fun t -> t.upgrade

let to_response = fun t ->
  if not t.sent && not t.halted then
    Web_server.Response.not_found
      ~headers:[ ("content-type", "text/plain; charset=utf-8"); ]
      ~body:"Not Found"
      ()
  else
    Web_server.Response.make t.resp_status ~headers:t.resp_headers ~body:t.resp_body ()

let assign_key = fun () -> TypedKeyHashMap.key ()

let assign = fun key value t ->
  let assigns = TypedKeyHashMap.from_list (TypedKeyHashMap.values t.assigns) in
  let _ = TypedKeyHashMap.insert assigns ~key ~value in
  { t with assigns }

let get_assign = fun key t -> TypedKeyHashMap.get t.assigns ~key
