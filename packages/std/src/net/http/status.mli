(** HTTP status codes **)

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

val of_int : int -> t
(** Create status code from integer **)

val to_int : t -> int
(** Convert status code to integer **)

val of_string : string -> (t, [ `InvalidStatus ]) result
(** Parse status code from string **)

val to_string : t -> string
(** Convert status code to string **)

val reason_phrase : t -> string
(** Get the standard reason phrase for a status code **)

val is_informational : t -> bool
(** Check if status is 1xx **)

val is_success : t -> bool
(** Check if status is 2xx **)

val is_redirection : t -> bool
(** Check if status is 3xx **)

val is_client_error : t -> bool
(** Check if status is 4xx **)

val is_server_error : t -> bool
(** Check if status is 5xx **)

val compare : t -> t -> int
(** Compare two status codes **)

val equal : t -> t -> bool
(** Check if two status codes are equal **)
