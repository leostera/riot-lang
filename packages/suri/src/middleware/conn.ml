open Std
open Std.Collections

type peer = { ip : Net.Addr.tcp_addr; port : int }

type upgrade_info = {
  opts : Channel.Handler.upgrade_opts;
  handler : Channel.Handler.t;
}

(** Extensible type for storing arbitrary data in connection *)
type assign_value = ..

type t = {
  socket_conn : Socket_pool.Connection.t;
  req : Web_server.Request.t;
  peer : peer;
  method_override : Net.Http.Method.t option;  (* For method override middleware *)
  params : (string * string) list;
  body_params : (string * string) list;
  resp_status : Net.Http.Status.t;
  resp_headers : (string * string) list;
  resp_body : string;
  halted : bool;
  sent : bool;
  upgrade : upgrade_info option;
  assigns : (string, assign_value) HashMap.t;
}

let make socket_conn req =
  let peer_addr = Socket_pool.Connection.peer socket_conn in
  let peer = { ip = Net.Addr.ip peer_addr; port = Net.Addr.port peer_addr } in
  {
    socket_conn;
    req;
    peer;
    method_override = None;
    params = [];
    body_params = [];
    resp_status = Ok;
    resp_headers = [];
    resp_body = "";
    halted = false;
    sent = false;
    upgrade = None;
    assigns = HashMap.create ();
  }

let method_ t = 
  match t.method_override with
  | Some m -> m
  | None -> Web_server.Request.method_ t.req

let uri t = Web_server.Request.uri t.req

let path t =
  let uri_str = Web_server.Request.uri t.req in
  match String.index_opt uri_str '?' with
  | Some idx -> String.sub uri_str 0 idx
  | None -> uri_str

let headers t = Web_server.Request.headers t.req
let body t = Web_server.Request.body t.req
let params t = t.params
let body_params t = t.body_params
let peer t = t.peer
let resp_headers t = t.resp_headers
let with_status status t = { t with resp_status = status }
let with_body body t = { t with resp_body = body }

let with_header name value t =
  { t with resp_headers = (name, value) :: t.resp_headers }

let with_method method_ t = { t with method_override = Some method_ }

let with_peer peer t = { t with peer }
let respond ~status ?body t =
  let t = with_status status t in
  match body with Some b -> with_body b t | None -> t

let send t = { t with sent = true }
let sent t = t.sent
let halt t = { t with halted = true }
let halted t = t.halted
let set_params params t = { t with params }
let set_body_params body_params t = { t with body_params }
let socket_conn t = t.socket_conn

let upgrade_websocket opts handler t =
  { t with upgrade = Some { opts; handler }; halted = true }

let get_upgrade t = t.upgrade

let to_response t =
  Web_server.Response.make t.resp_status ~headers:t.resp_headers
    ~body:t.resp_body ()

let assign key value t =
  let _ = HashMap.insert t.assigns key value in
  ()

let get_assign key t =
  HashMap.get t.assigns key
