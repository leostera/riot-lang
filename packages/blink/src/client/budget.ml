open Std

type policy = {
  capacity: int;
  window: Time.Duration.t;
}

type t = {
  capacity: int;
  window: Time.Duration.t;
  mutable remaining: int;
  mutable window_started_at: Time.Instant.t;
}

let policy = fun ~capacity ~window -> { capacity = Int.max 0 capacity; window }

let create = fun ~capacity ~window now ->
  {
    capacity = Int.max 0 capacity;
    window;
    remaining = Int.max 0 capacity;
    window_started_at = now;
  }

let create_with_policy = fun (budget_policy: policy) now ->
  create ~capacity:budget_policy.capacity ~window:budget_policy.window now

let capacity = fun value -> value.capacity

let remaining = fun value -> value.remaining

let reset_if_needed = fun ~now value ->
  let age = Time.Instant.saturating_duration_since ~earlier:value.window_started_at now in
  if Time.Duration.compare age value.window != Order.LT then
    (
      value.remaining <- value.capacity;
      value.window_started_at <- now
    )

let allow = fun ~now value ->
  reset_if_needed ~now value;
  if value.remaining <= 0 then
    false
  else
    (
      value.remaining <- value.remaining - 1;
      true
    )
