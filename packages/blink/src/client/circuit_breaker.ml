open Std

type state =
  | Closed
  | Open
  | HalfOpen

type policy = {
  failure_threshold: int;
  reset_after: Time.Duration.t;
}

type t = {
  failure_threshold: int;
  reset_after: Time.Duration.t;
  mutable state: state;
  mutable consecutive_failures: int;
  mutable opened_at: Time.Instant.t option;
}

let policy = fun ?(failure_threshold = 3) ?(reset_after = Time.Duration.from_secs 30) () -> {
  failure_threshold = Int.max 1 failure_threshold;
  reset_after;
}

let default_policy = policy ()

let create = fun ?(policy = default_policy) () ->
  {
    failure_threshold = policy.failure_threshold;
    reset_after = policy.reset_after;
    state = Closed;
    consecutive_failures = 0;
    opened_at = None;
  }

let state = fun value -> value.state

let state_to_string = fun value ->
  match value with
  | Closed -> "closed"
  | Open -> "open"
  | HalfOpen -> "half_open"

let consecutive_failures = fun value -> value.consecutive_failures

let open_circuit = fun ~now value ->
  value.state <- Open;
  value.opened_at <- Some now

let allow_request = fun ~now value ->
  match value.state with
  | Closed
  | HalfOpen -> true
  | Open -> (
      match value.opened_at with
      | None ->
          value.state <- HalfOpen;
          true
      | Some opened_at ->
          let elapsed = Time.Instant.saturating_duration_since ~earlier:opened_at now in
          if Time.Duration.compare elapsed value.reset_after != Order.LT then
            (
              value.state <- HalfOpen;
              true
            )
          else
            false
    )

let record_success = fun value ->
  value.state <- Closed;
  value.opened_at <- None;
  value.consecutive_failures <- 0

let record_failure = fun ~now value ->
  value.consecutive_failures <- value.consecutive_failures + 1;
  match value.state with
  | HalfOpen -> open_circuit ~now value
  | Closed
  | Open ->
      if value.consecutive_failures >= value.failure_threshold then
        open_circuit ~now value
