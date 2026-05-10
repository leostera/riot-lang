open Std

module Request = Super.Request
module Response = Super.Response
module Budget = Super.Budget
module Telemetry = Super.Telemetry

type transport = Request.t -> (Response.t, Error.t) result

type pool_config = {
  max_idle_per_endpoint: int;
  idle_ttl: Time.Duration.t option;
}

type connection_policy =
  | CloseAfterRequest
  | ReuseConnection
  | Pool of pool_config

type t = {
  now: unit -> Time.Instant.t;
  transport: transport option;
  connection_policy: connection_policy;
  budget_policy: Budget.policy;
  telemetry: Telemetry.t -> unit;
}

let pool = fun ?idle_ttl ~max_idle_per_endpoint () -> {
  max_idle_per_endpoint = Int.max 0 max_idle_per_endpoint;
  idle_ttl;
}

let connection_policy_to_string = fun value ->
  match value with
  | CloseAfterRequest -> "close_after_request"
  | ReuseConnection -> "reuse_connection"
  | Pool _ -> "pool"

let close_behavior = fun value ->
  match value with
  | CloseAfterRequest -> "transport_closes_after_request"
  | ReuseConnection -> "transport_may_reuse_connection"
  | Pool _ -> "transport_uses_connection_pool"

let default_budget_policy = Budget.policy ~capacity:100 ~window:(Time.Duration.from_secs 10)

let make = fun
  ?(now = Time.Instant.now)
  ?transport
  ?(connection_policy = CloseAfterRequest)
  ?(budget_policy = default_budget_policy)
  ?(telemetry = fun _ -> ())
  () ->
  {
    now;
    transport;
    connection_policy;
    budget_policy;
    telemetry;
  }
