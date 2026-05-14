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
  | RateLimitedResponse
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
  | RateLimitedResponse -> "rate_limited"
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

let error_class_from_transport_error = fun error ->
  match error with
  | Error.ProtocolError (Error.InvalidRequestUri _) -> InvalidRequest
  | Error.ProtocolError Error.RequestBudgetExhausted -> RateLimitedByBudget
  | Error.ProtocolError (Error.TransportRaised _ | Error.ApplicationTransportError _) ->
      UnknownError
  | Error.ProtocolError (Error.UnsupportedWebSocketScheme _
  | Error.EmptyChunkSize
  | Error.InvalidChunkSize
  | Error.ChunkSizeOverflow
  | Error.InvalidChunkDataLineEnding)
  | Error.ParseError _
  | Error.WebSocketParseError _
  | Error.WebSocketSerializeError _
  | Error.HandshakeFailed _
  | Error.InvalidFrame -> ResponseFailed
  | Error.RequestFailed _ -> RequestFailed
  | Error.ResponseFailed _ -> ResponseFailed
  | Error.NetError _
  | Error.TlsError _
  | Error.Eof
  | Error.Closed -> ConnectFailed
