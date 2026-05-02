open Std

module Request = Request
module Response = Response

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
}

let lifecycle_to_string = fun value ->
  match value with
  | Started -> "started"
  | Completed -> "completed"
  | Failed -> "failed"
  | Retrying -> "retrying"
  | Blocked -> "blocked"

let attempt = fun
  ~attempt
  ~started_at
  ~completed_at
  ~lifecycle
  ?status
  ?error_class
  ?error_message
  ?planned_backoff
  () ->
  {
    attempt;
    started_at;
    completed_at;
    latency = Time.Instant.saturating_duration_since ~earlier:started_at completed_at;
    lifecycle;
    status;
    status_class = Option.map status ~fn:Response.status_class;
    error_class;
    error_message;
    planned_backoff;
  }

let make = fun
  ~request
  ~started_at
  ~completed_at
  ~attempts
  ?final_status
  ?final_error_class
  ~connection_policy
  ~close_behavior
  ?budget_remaining
  () ->
  {
    request;
    started_at;
    completed_at;
    total_latency = Time.Instant.saturating_duration_since ~earlier:started_at completed_at;
    attempts;
    final_status;
    final_error_class;
    connection_policy;
    close_behavior;
    budget_remaining;
  }
