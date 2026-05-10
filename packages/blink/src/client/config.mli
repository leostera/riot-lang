(** Request transport used by a managed Blink client. *)
type transport = Request.t -> (Response.t, Error.t) Std.result
(** Connection-pool limits for the managed low-level Blink transport. *)
type pool_config = {
  max_idle_per_endpoint: int;
  idle_ttl: Std.Time.Duration.t option;
}
(** Connection policy for requests sent through `Blink.Client`. *)
type connection_policy =
  | CloseAfterRequest
  | ReuseConnection
  | Pool of pool_config
(** Managed client configuration. *)
type t = {
  now: unit -> Std.Time.Instant.t;
  transport: transport option;
  connection_policy: connection_policy;
  budget_policy: Budget.policy;
  telemetry: Telemetry.t -> unit;
}

val pool : ?idle_ttl:Std.Time.Duration.t -> max_idle_per_endpoint:int -> unit -> pool_config

val connection_policy_to_string : connection_policy -> string

val close_behavior : connection_policy -> string

val default_budget_policy : Budget.policy

val make :
  ?now:(unit -> Std.Time.Instant.t) ->
  ?transport:transport ->
  ?connection_policy:connection_policy ->
  ?budget_policy:Budget.policy ->
  ?telemetry:(Telemetry.t -> unit) ->
  unit ->
  t
