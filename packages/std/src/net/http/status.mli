(**
   HTTP status codes.

   HTTP status code types and utilities following RFC 7231. Includes all
   standard status codes from 1xx through 5xx ranges.

   ## Examples

   Basic usage:

   ```ocaml open Std.Net.Http

   let status = Status.Ok in Status.to_int status (* 200 *)
   Status.reason_phrase status (* "OK" *)

   let status = Status.from_int 404 in (* Status.NotFound *) ```

   Checking status categories:

   ```ocaml Status.is_success Status.Ok (* true *) Status.is_client_error
   Status.NotFound (* true *) Status.is_server_error Status.InternalServerError
   (* true *)

   (* Handling responses *) let handle_response status body = if
   Status.is_success status then process_success body else if
   Status.is_client_error status then Log.warn "Client error: %s"
   (Status.reason_phrase status) else Log.error "Server error: %d"
   (Status.to_int status) ```

   Custom status codes:

   ```ocaml (* Non-standard codes *) let custom = Status.Extension 999 in
   Status.to_int custom (* 999 *) ```

   ## Status Code Categories

   - **1xx Informational**: Request received, continuing process
   - **2xx Success**: Request successfully received, understood, and accepted
   - **3xx Redirection**: Further action needed to complete request
   - **4xx Client Error**: Request contains bad syntax or cannot be fulfilled
   - **5xx Server Error**: Server failed to fulfill valid request
*)
open Global

type t =
  (* 1xx Informational *)
  | Continue
  (** 100 - Continue with request *)
  | SwitchingProtocols
  (** 101 - Switching to new protocol *)
  | Processing
  (** 102 - Processing (WebDAV) *)
  | EarlyHints
  (** 103 - Early hints *)
  (* 2xx Success *)
  | Ok
  (** 200 - Request succeeded *)
  | Created
  (** 201 - Resource created *)
  | Accepted
  (** 202 - Accepted for processing *)
  | NonAuthoritativeInformation
  (** 203 - Non-authoritative information *)
  | NoContent
  (** 204 - No content to send *)
  | ResetContent
  (** 205 - Reset document view *)
  | PartialContent
  (** 206 - Partial content (range request) *)
  | MultiStatus
  (** 207 - Multi-status (WebDAV) *)
  | AlreadyReported
  (** 208 - Already reported (WebDAV) *)
  | ImUsed
  (** 226 - IM used (delta encoding) *)
  (* 3xx Redirection *)
  | MultipleChoices
  (** 300 - Multiple choices available *)
  | MovedPermanently
  (** 301 - Resource moved permanently *)
  | Found
  (** 302 - Resource found at different URI *)
  | SeeOther
  (** 303 - See other URI *)
  | NotModified
  (** 304 - Resource not modified *)
  | UseProxy
  (** 305 - Must use proxy *)
  | TemporaryRedirect
  (** 307 - Temporary redirect *)
  | PermanentRedirect
  (** 308 - Permanent redirect *)
  (* 4xx Client Error *)
  | BadRequest
  (** 400 - Malformed request *)
  | Unauthorized
  (** 401 - Authentication required *)
  | PaymentRequired
  (** 402 - Payment required *)
  | Forbidden
  (** 403 - Access forbidden *)
  | NotFound
  (** 404 - Resource not found *)
  | MethodNotAllowed
  (** 405 - Method not allowed *)
  | NotAcceptable
  (** 406 - Not acceptable *)
  | ProxyAuthenticationRequired
  (** 407 - Proxy authentication required *)
  | RequestTimeout
  (** 408 - Request timeout *)
  | Conflict
  (** 409 - Conflict with current state *)
  | Gone
  (** 410 - Resource permanently gone *)
  | LengthRequired
  (** 411 - Content-Length required *)
  | PreconditionFailed
  (** 412 - Precondition failed *)
  | PayloadTooLarge
  (** 413 - Payload too large *)
  | UriTooLong
  (** 414 - URI too long *)
  | UnsupportedMediaType
  (** 415 - Unsupported media type *)
  | RangeNotSatisfiable
  (** 416 - Range not satisfiable *)
  | ExpectationFailed
  (** 417 - Expectation failed *)
  | ImATeapot
  (** 418 - I'm a teapot (RFC 2324) *)
  | MisdirectedRequest
  (** 421 - Misdirected request *)
  | UnprocessableEntity
  (** 422 - Unprocessable entity (WebDAV) *)
  | Locked
  (** 423 - Resource locked (WebDAV) *)
  | FailedDependency
  (** 424 - Failed dependency (WebDAV) *)
  | TooEarly
  (** 425 - Too early *)
  | UpgradeRequired
  (** 426 - Upgrade required *)
  | PreconditionRequired
  (** 428 - Precondition required *)
  | TooManyRequests
  (** 429 - Too many requests *)
  | RequestHeaderFieldsTooLarge
  (** 431 - Request header fields too large *)
  | UnavailableForLegalReasons
  (** 451 - Unavailable for legal reasons *)
  (* 5xx Server Error *)
  | InternalServerError
  (** 500 - Internal server error *)
  | NotImplemented
  (** 501 - Not implemented *)
  | BadGateway
  (** 502 - Bad gateway *)
  | ServiceUnavailable
  (** 503 - Service unavailable *)
  | GatewayTimeout
  (** 504 - Gateway timeout *)
  | HttpVersionNotSupported
  (** 505 - HTTP version not supported *)
  | VariantAlsoNegotiates
  (** 506 - Variant also negotiates *)
  | InsufficientStorage
  (** 507 - Insufficient storage (WebDAV) *)
  | LoopDetected
  (** 508 - Loop detected (WebDAV) *)
  | NotExtended
  (** 510 - Not extended *)
  | NetworkAuthenticationRequired
  (** 511 - Network authentication required *)
  (* Extension *)
  (** Custom or non-standard status code. *)
  | Extension of int
type error =
  | InvalidStatus

(**
   Creates status code from integer value.

   ## Examples

   ```ocaml Status.from_int 200 (* Ok *) Status.from_int 404 (* NotFound *)
   Status.from_int 999 (* Extension 999 *) ```
*)
val from_int: int -> t

(**
   Converts status code to integer value.

   ## Examples

   ```ocaml Status.to_int Status.Ok (* 200 *) Status.to_int Status.NotFound (*
   404 *) ```
*)
val to_int: t -> int

(**
   Parses status code from string representation.

   ## Examples

   ```ocaml Status.from_string "200" (* Ok (Ok) *) Status.from_string "404" (* Ok
   (NotFound) *) Status.from_string "abc" (* Error InvalidStatus *) ```
*)
val from_string: string -> (t, error) Kernel.result

(**
   Converts status code to string representation of the integer.

   ## Examples

   ```ocaml Status.to_string Status.Ok (* "200" *) Status.to_string
   Status.NotFound (* "404" *) ```
*)
val to_string: t -> string

(**
   Returns [true] when two statuses represent the same numeric HTTP status
   code.

   Extension statuses compare by their integer code, so [Status.Extension 599]
   is equal to any other status value that renders as [599].
*)
val equal: t -> t -> bool

(**
   Returns the standard reason phrase for a status code.

   ## Examples

   ```ocaml Status.reason_phrase Status.Ok (* "OK" *) Status.reason_phrase
   Status.NotFound (* "Not Found" *) Status.reason_phrase
   Status.InternalServerError (* "Internal Server Error" *) ```
*)
val reason_phrase: t -> string

(**
   Returns [true] if status code is in 1xx range (informational).

   ## Examples

   ```ocaml Status.is_informational Status.Continue (* true *)
   Status.is_informational Status.Ok (* false *) ```
*)
val is_informational: t -> bool

(**
   Returns [true] if status code is in 2xx range (success).

   ## Examples

   ```ocaml Status.is_success Status.Ok (* true *) Status.is_success
   Status.Created (* true *) Status.is_success Status.NotFound (* false *) ```
*)
val is_success: t -> bool

(**
   Returns [true] if status code is in 3xx range (redirection).

   ## Examples

   ```ocaml Status.is_redirection Status.MovedPermanently (* true *)
   Status.is_redirection Status.Found (* true *) Status.is_redirection
   Status.Ok (* false *) ```
*)
val is_redirection: t -> bool

(**
   Returns [true] if status code is in 4xx range (client error).

   ## Examples

   ```ocaml Status.is_client_error Status.BadRequest (* true *)
   Status.is_client_error Status.NotFound (* true *) Status.is_client_error
   Status.InternalServerError (* false *) ```
*)
val is_client_error: t -> bool

(**
   Returns [true] if status code is in 5xx range (server error).

   ## Examples

   ```ocaml Status.is_server_error Status.InternalServerError (* true *)
   Status.is_server_error Status.NotImplemented (* true *)
   Status.is_server_error Status.NotFound (* false *) ```
*)
val is_server_error: t -> bool

(**
   Compares two status codes by their integer values.

   ## Examples

   ```ocaml Status.compare Status.Ok Status.NotFound (* < 0, since 200 < 404 *)
   ```
*)
val compare: t -> t -> Order.t

(**
   Checks if two status codes are equal.

   ## Examples

   ```ocaml Status.equal Status.Ok Status.Ok (* true *) Status.equal Status.Ok
   Status.NotFound (* false *) ```
*)
val equal: t -> t -> bool
