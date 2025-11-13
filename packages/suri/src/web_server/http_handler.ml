open Std

type protocol_upgrade = WebSocket of Channel.Handler.upgrade_opts * Channel.Handler.t 

type response =
  | Response of Response.t
  | Upgrade of protocol_upgrade

type t = Socket_pool.Connection.t -> Request.t -> response

let respond res = Response res
let upgrade opts handler = 
  Log.info "Http_handler.websocket: Creating WebSocket upgrade response";
  Upgrade (WebSocket (opts, handler))
