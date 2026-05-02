open Std

type status_class =
  | Informational
  | Success
  | Redirect
  | ClientError
  | RateLimited
  | ServerError
  | UnknownStatus

type error_class =
  | InvalidRequest
  | ConnectFailed
  | RequestFailed
  | ResponseFailed
  | DeadlineExceeded
  | RateLimitedByBudget
  | ServerRejected
  | UnknownError

type t = {
  status: int;
  body: string;
  headers: (string * string) list;
}

let make = fun ?(headers = []) ~status ~body () -> { status; body; headers }

let status_class = fun status ->
  if status = 429 then
    RateLimited
  else if status >= 100 && status < 200 then
    Informational
  else if status >= 200 && status < 300 then
    Success
  else if status >= 300 && status < 400 then
    Redirect
  else if status >= 400 && status < 500 then
    ClientError
  else if status >= 500 && status < 600 then
    ServerError
  else
    UnknownStatus

let status_class_to_string = fun value ->
  match value with
  | Informational -> "informational"
  | Success -> "success"
  | Redirect -> "redirect"
  | ClientError -> "client_error"
  | RateLimited -> "rate_limited"
  | ServerError -> "server_error"
  | UnknownStatus -> "unknown_status"

let error_class_to_string = fun value ->
  match value with
  | InvalidRequest -> "invalid_request"
  | ConnectFailed -> "connect_failed"
  | RequestFailed -> "request_failed"
  | ResponseFailed -> "response_failed"
  | DeadlineExceeded -> "deadline_exceeded"
  | RateLimitedByBudget -> "rate_limited_by_budget"
  | ServerRejected -> "server_rejected"
  | UnknownError -> "unknown_error"

let is_success = fun response ->
  match status_class response.status with
  | Success -> true
  | Informational
  | Redirect
  | ClientError
  | RateLimited
  | ServerError
  | UnknownStatus -> false

let retryable_status = fun status ->
  match status_class status with
  | RateLimited
  | ServerError -> true
  | Informational
  | Success
  | Redirect
  | ClientError
  | UnknownStatus -> false

let error_class_of_transport_error = fun message ->
  let normalized = String.lowercase_ascii message in
  if String.contains normalized "invalid" then
    InvalidRequest
  else if String.contains normalized "deadline" then
    DeadlineExceeded
  else if String.contains normalized "connect" then
    ConnectFailed
  else if String.contains normalized "request" then
    RequestFailed
  else if String.contains normalized "response" then
    ResponseFailed
  else
    UnknownError
