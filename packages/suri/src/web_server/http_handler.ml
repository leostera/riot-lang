open Std

type protocol_upgrade =
  WebSocket of Channel.Handler.upgrade_opts * Channel.Handler.t

type response =
  | Response of Response.t
  | Upgrade of protocol_upgrade

type t = Socket_pool.Connection.t -> Request.t -> response

let respond = fun res -> Response res

let upgrade = fun opts handler ->
  Log.info "Http_handler.websocket: Creating WebSocket upgrade response";
  Upgrade (WebSocket (opts, handler))
