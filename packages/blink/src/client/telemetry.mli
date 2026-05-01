open Std

type lifecycle =
  | Started
  | Completed
  | Failed
  | Retrying
  | Blocked
type attempt = {
  attempt: int;
  started_at: Time.Instant.t;
  completed_at: Time.Instant.t;
  latency: Time.Duration.t;
  lifecycle: lifecycle;
  status: int option;
  status_class: Response.status_class option;
  error_class: Response.error_class option;
  error_message: string option;
  planned_backoff: Time.Duration.t option;
}
type t = {
  request: Request.t;
  started_at: Time.Instant.t;
  completed_at: Time.Instant.t;
  total_latency: Time.Duration.t;
  attempts: attempt list;
  final_status: int option;
  final_error_class: Response.error_class option;
  connection_policy: string;
  close_behavior: string;
  budget_remaining: int option;
  circuit_state: Circuit_breaker.state;
}

val lifecycle_to_string: lifecycle -> string

val attempt:
  attempt:int ->
  started_at:Time.Instant.t ->
  completed_at:Time.Instant.t ->
  lifecycle:lifecycle ->
  ?status:int ->
  ?error_class:Response.error_class ->
  ?error_message:string ->
  ?planned_backoff:Time.Duration.t ->
  unit ->
  attempt

val make:
  request:Request.t ->
  started_at:Time.Instant.t ->
  completed_at:Time.Instant.t ->
  attempts:attempt list ->
  ?final_status:int ->
  ?final_error_class:Response.error_class ->
  connection_policy:string ->
  close_behavior:string ->
  ?budget_remaining:int ->
  circuit_state:Circuit_breaker.state ->
  unit ->
  t
