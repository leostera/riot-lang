open Std

type protocol_upgrade =
  WebSocket of Channel.Handler.upgrade_opts * Channel.Handler.t
(** Protocol upgrade (e.g., WebSocket) *)
(** Handler response type that supports protocol upgrades *)
type response =
  | Response of Response.t
  (** Normal HTTP response *)
  | Upgrade of protocol_upgrade
type t = Socket_pool.Connection.t -> Request.t -> response
(** Handler function that can return either a normal response or trigger an upgrade *)
val respond: Response.t -> response
(** Create a normal HTTP response *)
val upgrade: Channel.Handler.upgrade_opts -> Channel.Handler.t -> response
(** Create a WebSocket upgrade response *)
