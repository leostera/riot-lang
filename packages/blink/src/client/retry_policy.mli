open Std

type policy = {
  max_attempts: int;
  base_delay: Time.Duration.t;
  max_delay: Time.Duration.t;
  jitter_nanos: int64;
}

val default: policy

val make:
  ?max_attempts:int ->
  ?base_delay:Time.Duration.t ->
  ?max_delay:Time.Duration.t ->
  ?jitter_nanos:int64 ->
  unit ->
  policy

val should_retry_status: policy -> attempt:int -> int -> bool

val should_retry_error: policy -> attempt:int -> Response.error_class -> bool

val delay_for_attempt: policy -> attempt:int -> Time.Duration.t
