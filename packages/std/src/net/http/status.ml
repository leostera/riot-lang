open Global
open Kernel

type t =
  (* 1xx Informational *)
  | Continue
  | SwitchingProtocols
  | Processing
  | EarlyHints
  (* 2xx Success *)
  | Ok
  | Created
  | Accepted
  | NonAuthoritativeInformation
  | NoContent
  | ResetContent
  | PartialContent
  | MultiStatus
  | AlreadyReported
  | ImUsed
  (* 3xx Redirection *)
  | MultipleChoices
  | MovedPermanently
  | Found
  | SeeOther
  | NotModified
  | UseProxy
  | TemporaryRedirect
  | PermanentRedirect
  (* 4xx Client Error *)
  | BadRequest
  | Unauthorized
  | PaymentRequired
  | Forbidden
  | NotFound
  | MethodNotAllowed
  | NotAcceptable
  | ProxyAuthenticationRequired
  | RequestTimeout
  | Conflict
  | Gone
  | LengthRequired
  | PreconditionFailed
  | PayloadTooLarge
  | UriTooLong
  | UnsupportedMediaType
  | RangeNotSatisfiable
  | ExpectationFailed
  | ImATeapot
  | MisdirectedRequest
  | UnprocessableEntity
  | Locked
  | FailedDependency
  | TooEarly
  | UpgradeRequired
  | PreconditionRequired
  | TooManyRequests
  | RequestHeaderFieldsTooLarge
  | UnavailableForLegalReasons
  (* 5xx Server Error *)
  | InternalServerError
  | NotImplemented
  | BadGateway
  | ServiceUnavailable
  | GatewayTimeout
  | HttpVersionNotSupported
  | VariantAlsoNegotiates
  | InsufficientStorage
  | LoopDetected
  | NotExtended
  | NetworkAuthenticationRequired
  (* Extension *)
  | Extension of int

type error =
  | InvalidStatus

let to_int = fun __tmp1 ->
  match __tmp1 with
  | Continue -> 100
  | SwitchingProtocols -> 101
  | Processing -> 102
  | EarlyHints -> 103
  | Ok -> 200
  | Created -> 201
  | Accepted -> 202
  | NonAuthoritativeInformation -> 203
  | NoContent -> 204
  | ResetContent -> 205
  | PartialContent -> 206
  | MultiStatus -> 207
  | AlreadyReported -> 208
  | ImUsed -> 226
  | MultipleChoices -> 300
  | MovedPermanently -> 301
  | Found -> 302
  | SeeOther -> 303
  | NotModified -> 304
  | UseProxy -> 305
  | TemporaryRedirect -> 307
  | PermanentRedirect -> 308
  | BadRequest -> 400
  | Unauthorized -> 401
  | PaymentRequired -> 402
  | Forbidden -> 403
  | NotFound -> 404
  | MethodNotAllowed -> 405
  | NotAcceptable -> 406
  | ProxyAuthenticationRequired -> 407
  | RequestTimeout -> 408
  | Conflict -> 409
  | Gone -> 410
  | LengthRequired -> 411
  | PreconditionFailed -> 412
  | PayloadTooLarge -> 413
  | UriTooLong -> 414
  | UnsupportedMediaType -> 415
  | RangeNotSatisfiable -> 416
  | ExpectationFailed -> 417
  | ImATeapot -> 418
  | MisdirectedRequest -> 421
  | UnprocessableEntity -> 422
  | Locked -> 423
  | FailedDependency -> 424
  | TooEarly -> 425
  | UpgradeRequired -> 426
  | PreconditionRequired -> 428
  | TooManyRequests -> 429
  | RequestHeaderFieldsTooLarge -> 431
  | UnavailableForLegalReasons -> 451
  | InternalServerError -> 500
  | NotImplemented -> 501
  | BadGateway -> 502
  | ServiceUnavailable -> 503
  | GatewayTimeout -> 504
  | HttpVersionNotSupported -> 505
  | VariantAlsoNegotiates -> 506
  | InsufficientStorage -> 507
  | LoopDetected -> 508
  | NotExtended -> 510
  | NetworkAuthenticationRequired -> 511
  | Extension code -> code

let from_int = fun __tmp1 ->
  match __tmp1 with
  | 100 -> Continue
  | 101 -> SwitchingProtocols
  | 102 -> Processing
  | 103 -> EarlyHints
  | 200 -> Ok
  | 201 -> Created
  | 202 -> Accepted
  | 203 -> NonAuthoritativeInformation
  | 204 -> NoContent
  | 205 -> ResetContent
  | 206 -> PartialContent
  | 207 -> MultiStatus
  | 208 -> AlreadyReported
  | 226 -> ImUsed
  | 300 -> MultipleChoices
  | 301 -> MovedPermanently
  | 302 -> Found
  | 303 -> SeeOther
  | 304 -> NotModified
  | 305 -> UseProxy
  | 307 -> TemporaryRedirect
  | 308 -> PermanentRedirect
  | 400 -> BadRequest
  | 401 -> Unauthorized
  | 402 -> PaymentRequired
  | 403 -> Forbidden
  | 404 -> NotFound
  | 405 -> MethodNotAllowed
  | 406 -> NotAcceptable
  | 407 -> ProxyAuthenticationRequired
  | 408 -> RequestTimeout
  | 409 -> Conflict
  | 410 -> Gone
  | 411 -> LengthRequired
  | 412 -> PreconditionFailed
  | 413 -> PayloadTooLarge
  | 414 -> UriTooLong
  | 415 -> UnsupportedMediaType
  | 416 -> RangeNotSatisfiable
  | 417 -> ExpectationFailed
  | 418 -> ImATeapot
  | 421 -> MisdirectedRequest
  | 422 -> UnprocessableEntity
  | 423 -> Locked
  | 424 -> FailedDependency
  | 425 -> TooEarly
  | 426 -> UpgradeRequired
  | 428 -> PreconditionRequired
  | 429 -> TooManyRequests
  | 431 -> RequestHeaderFieldsTooLarge
  | 451 -> UnavailableForLegalReasons
  | 500 -> InternalServerError
  | 501 -> NotImplemented
  | 502 -> BadGateway
  | 503 -> ServiceUnavailable
  | 504 -> GatewayTimeout
  | 505 -> HttpVersionNotSupported
  | 506 -> VariantAlsoNegotiates
  | 507 -> InsufficientStorage
  | 508 -> LoopDetected
  | 510 -> NotExtended
  | 511 -> NetworkAuthenticationRequired
  | code -> Extension code

let from_string: string -> (t, error) Kernel.result = fun s ->
  match Int.parse s with
  | Some code -> Ok (from_int code)
  | None -> Error InvalidStatus

let to_string = fun status -> Int.to_string (to_int status)

let equal = fun a b -> to_int a = to_int b

let reason_phrase = fun __tmp1 ->
  match __tmp1 with
  | Continue -> "Continue"
  | SwitchingProtocols -> "Switching Protocols"
  | Processing -> "Processing"
  | EarlyHints -> "Early Hints"
  | Ok -> "OK"
  | Created -> "Created"
  | Accepted -> "Accepted"
  | NonAuthoritativeInformation -> "Non-Authoritative Information"
  | NoContent -> "No Content"
  | ResetContent -> "Reset Content"
  | PartialContent -> "Partial Content"
  | MultiStatus -> "Multi-Status"
  | AlreadyReported -> "Already Reported"
  | ImUsed -> "IM Used"
  | MultipleChoices -> "Multiple Choices"
  | MovedPermanently -> "Moved Permanently"
  | Found -> "Found"
  | SeeOther -> "See Other"
  | NotModified -> "Not Modified"
  | UseProxy -> "Use Proxy"
  | TemporaryRedirect -> "Temporary Redirect"
  | PermanentRedirect -> "Permanent Redirect"
  | BadRequest -> "Bad Request"
  | Unauthorized -> "Unauthorized"
  | PaymentRequired -> "Payment Required"
  | Forbidden -> "Forbidden"
  | NotFound -> "Not Found"
  | MethodNotAllowed -> "Method Not Allowed"
  | NotAcceptable -> "Not Acceptable"
  | ProxyAuthenticationRequired -> "Proxy Authentication Required"
  | RequestTimeout -> "Request Timeout"
  | Conflict -> "Conflict"
  | Gone -> "Gone"
  | LengthRequired -> "Length Required"
  | PreconditionFailed -> "Precondition Failed"
  | PayloadTooLarge -> "Payload Too Large"
  | UriTooLong -> "URI Too Long"
  | UnsupportedMediaType -> "Unsupported Media Type"
  | RangeNotSatisfiable -> "Range Not Satisfiable"
  | ExpectationFailed -> "Expectation Failed"
  | ImATeapot -> "I'm a teapot"
  | MisdirectedRequest -> "Misdirected Request"
  | UnprocessableEntity -> "Unprocessable Entity"
  | Locked -> "Locked"
  | FailedDependency -> "Failed Dependency"
  | TooEarly -> "Too Early"
  | UpgradeRequired -> "Upgrade Required"
  | PreconditionRequired -> "Precondition Required"
  | TooManyRequests -> "Too Many Requests"
  | RequestHeaderFieldsTooLarge -> "Request Header Fields Too Large"
  | UnavailableForLegalReasons -> "Unavailable For Legal Reasons"
  | InternalServerError -> "Internal Server Error"
  | NotImplemented -> "Not Implemented"
  | BadGateway -> "Bad Gateway"
  | ServiceUnavailable -> "Service Unavailable"
  | GatewayTimeout -> "Gateway Timeout"
  | HttpVersionNotSupported -> "HTTP Version Not Supported"
  | VariantAlsoNegotiates -> "Variant Also Negotiates"
  | InsufficientStorage -> "Insufficient Storage"
  | LoopDetected -> "Loop Detected"
  | NotExtended -> "Not Extended"
  | NetworkAuthenticationRequired -> "Network Authentication Required"
  | Extension code -> Int.to_string code

let is_informational = fun status ->
  let code = to_int status in
  code >= 100 && code < 200

let is_success = fun status ->
  let code = to_int status in
  code >= 200 && code < 300

let is_redirection = fun status ->
  let code = to_int status in
  code >= 300 && code < 400

let is_client_error = fun status ->
  let code = to_int status in
  code >= 400 && code < 500

let is_server_error = fun status ->
  let code = to_int status in
  code >= 500 && code < 600

let compare = fun s1 s2 -> Int.compare (to_int s1) (to_int s2)

let equal = fun s1 s2 ->
  match compare s1 s2 with
  | Order.EQ -> true
  | Order.LT
  | Order.GT -> false
