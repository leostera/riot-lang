open Std

module Response = Response

type policy = {
  max_attempts: int;
  base_delay: Time.Duration.t;
  max_delay: Time.Duration.t;
  jitter_nanos: int64;
}

let make = fun
  ?(max_attempts = 3)
  ?(base_delay = Time.Duration.from_millis 100)
  ?(max_delay = Time.Duration.from_secs 2)
  ?(jitter_nanos = 25_000_000L)
  () ->
  {
    max_attempts = Int.max 1 max_attempts;
    base_delay;
    max_delay;
    jitter_nanos;
  }

let default = make ()

let can_retry = fun policy ~attempt -> attempt < policy.max_attempts

let should_retry_status = fun policy ~attempt status ->
  can_retry policy ~attempt && Response.retryable_status status

let should_retry_error = fun policy ~attempt error_class ->
  if not (can_retry policy ~attempt) then
    false
  else
    match error_class with
    | Response.ConnectFailed
    | Response.RequestFailed
    | Response.ResponseFailed
    | Response.UnknownError -> true
    | Response.InvalidRequest
    | Response.DeadlineExceeded
    | Response.RateLimitedByBudget
    | Response.ServerRejected -> false

let delay_for_attempt = fun policy ~attempt ->
  let multiplier = Int.max 1 (1 lsl Int.max 0 (attempt - 1)) in
  let base = Time.Duration.saturating_mul policy.base_delay multiplier in
  let jitter =
    if policy.jitter_nanos <= 0L then
      Time.Duration.zero
    else
      let bounded = Int64.rem (Int64.from_int (attempt * 7_919)) policy.jitter_nanos in
      Time.Duration.from_nanos (Int64.to_int bounded)
  in
  Time.Duration.min policy.max_delay (Time.Duration.add base jitter)
