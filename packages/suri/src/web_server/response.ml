open Std

type t = {
  status: Net.Http.Status.t;
  headers: Net.Http.Header.t;
  version: Net.Http.Version.t;
  body: string;
}

let make = fun status ?(headers = []) ?(version = Net.Http.Version.Http11) ?(body = "") () ->
  {
    status;
    version;
    headers = Net.Http.Header.from_list headers;
    body;
  }

type response =
  ?headers:(string * string) list ->
  ?version:Net.Http.Version.t ->
  ?body:string ->
  unit ->
  t

let continue = make Continue

let switching_protocols = make SwitchingProtocols

let processing = make Processing

let early_hints = make EarlyHints

let ok = make Ok

let created = make Created

let accepted = make Accepted

let non_authoritative_information = make NonAuthoritativeInformation

let no_content = make NoContent

let reset_content = make ResetContent

let partial_content = make PartialContent

let multi_status = make MultiStatus

let already_reported = make AlreadyReported

let im_used = make ImUsed

let multiple_choices = make MultipleChoices

let moved_permanently = make MovedPermanently

let found = make Found

let see_other = make SeeOther

let not_modified = make NotModified

let use_proxy = make UseProxy

let temporary_redirect = make TemporaryRedirect

let permanent_redirect = make PermanentRedirect

let bad_request = make BadRequest

let unauthorized = make Unauthorized

let payment_required = make PaymentRequired

let forbidden = make Forbidden

let not_found = make NotFound

let method_not_allowed = make MethodNotAllowed

let not_acceptable = make NotAcceptable

let proxy_authentication_required = make ProxyAuthenticationRequired

let request_timeout = make RequestTimeout

let conflict = make Conflict

let gone = make Gone

let length_required = make LengthRequired

let precondition_failed = make PreconditionFailed

let request_entity_too_large = make PayloadTooLarge

let request_uri_too_long = make UriTooLong

let unsupported_media_type = make UnsupportedMediaType

let requested_range_not_satisfiable = make RangeNotSatisfiable

let expectation_failed = make ExpectationFailed

let im_a_teapot = make ImATeapot

let misdirected_request = make MisdirectedRequest

let unprocessable_entity = make UnprocessableEntity

let locked = make Locked

let failed_dependency = make FailedDependency

let too_early = make TooEarly

let upgrade_required = make UpgradeRequired

let precondition_required = make PreconditionRequired

let too_many_requests = make TooManyRequests

let request_header_fields_too_large = make RequestHeaderFieldsTooLarge

let unavailable_for_legal_reasons = make UnavailableForLegalReasons

let internal_server_error = make InternalServerError

let not_implemented = make NotImplemented

let bad_gateway = make BadGateway

let service_unavailable = make ServiceUnavailable

let gateway_timeout = make GatewayTimeout

let http_version_not_supported = make HttpVersionNotSupported

let variant_also_negotiates = make VariantAlsoNegotiates

let insufficient_storage = make InsufficientStorage

let loop_detected = make LoopDetected

let not_extended = make NotExtended

let network_authentication_required = make NetworkAuthenticationRequired

let bandwidth_limit_exceeded = make (Extension 509)

let blocked_by_windows_parental_controls = make (Extension 450)

let checkpoint = make (Extension 103)

let client_closed_request = make (Extension 499)

let enhance_your_calm = make (Extension 420)

let no_response = make (Extension 444)

let retry_with = make (Extension 449)

let switch_proxy = make (Extension 306)

let wrong_exchange_server = make (Extension 451)

let network_read_timeout_error = make (Extension 598)

let network_connect_timeout_error = make (Extension 599)
