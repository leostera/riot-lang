(** # HTTP Response Construction

    Provides a builder interface for HTTP responses with status codes, headers,
    and bodies.

    ## Example

    ```ocaml
    let response = Response.ok ~body:"Hello, World!" ()
    
    let custom = Response.make `OK
      ~headers:[("Content-Type", "application/json")]
      ~body:"{\"status\":\"ok\"}"
      ()
    ``` *)

open Std

type t = {
  status : Net.Http.Status.t;
  headers : Net.Http.Header.t;
  version : Net.Http.Version.t;
  body : string;
}
(** HTTP response type *)

val make :
  Net.Http.Status.t ->
  ?headers:(string * string) list ->
  ?version:Net.Http.Version.t ->
  ?body:string ->
  unit ->
  t
(** Create an HTTP response.

    - `status` - HTTP status code
    - `headers` - Optional list of (name, value) header pairs (default: `[]`)
    - `version` - HTTP version (default: `Http11`)
    - `body` - Response body (default: empty string) *)

type response =
  ?headers:(string * string) list ->
  ?version:Net.Http.Version.t ->
  ?body:string ->
  unit ->
  t
(** Response builder function type *)

(** ## Success Responses (2xx) *)

val ok : response
(** `200 OK` *)

val created : response
(** `201 Created` *)

val accepted : response
(** `202 Accepted` *)

val non_authoritative_information : response
(** `203 Non-Authoritative Information` *)

val no_content : response
(** `204 No Content` *)

val reset_content : response
(** `205 Reset Content` *)

val partial_content : response
(** `206 Partial Content` *)

val multi_status : response
(** `207 Multi-Status` (WebDAV) *)

val already_reported : response
(** `208 Already Reported` (WebDAV) *)

val im_used : response
(** `226 IM Used` *)

(** ## Redirection Responses (3xx) *)

val multiple_choices : response
(** `300 Multiple Choices` *)

val moved_permanently : response
(** `301 Moved Permanently` *)

val found : response
(** `302 Found` *)

val see_other : response
(** `303 See Other` *)

val not_modified : response
(** `304 Not Modified` *)

val use_proxy : response
(** `305 Use Proxy` *)

val switch_proxy : response
(** `306 Switch Proxy` *)

val temporary_redirect : response
(** `307 Temporary Redirect` *)

val permanent_redirect : response
(** `308 Permanent Redirect` *)

(** ## Client Error Responses (4xx) *)

val bad_request : response
(** `400 Bad Request` *)

val unauthorized : response
(** `401 Unauthorized` *)

val payment_required : response
(** `402 Payment Required` *)

val forbidden : response
(** `403 Forbidden` *)

val not_found : response
(** `404 Not Found` *)

val method_not_allowed : response
(** `405 Method Not Allowed` *)

val not_acceptable : response
(** `406 Not Acceptable` *)

val proxy_authentication_required : response
(** `407 Proxy Authentication Required` *)

val request_timeout : response
(** `408 Request Timeout` *)

val conflict : response
(** `409 Conflict` *)

val gone : response
(** `410 Gone` *)

val length_required : response
(** `411 Length Required` *)

val precondition_failed : response
(** `412 Precondition Failed` *)

val request_entity_too_large : response
(** `413 Request Entity Too Large` *)

val request_uri_too_long : response
(** `414 Request-URI Too Long` *)

val unsupported_media_type : response
(** `415 Unsupported Media Type` *)

val requested_range_not_satisfiable : response
(** `416 Requested Range Not Satisfiable` *)

val expectation_failed : response
(** `417 Expectation Failed` *)

val im_a_teapot : response
(** `418 I'm a teapot` *)

val enhance_your_calm : response
(** `420 Enhance Your Calm` (Twitter) *)

val unprocessable_entity : response
(** `422 Unprocessable Entity` (WebDAV) *)

val locked : response
(** `423 Locked` (WebDAV) *)

val failed_dependency : response
(** `424 Failed Dependency` (WebDAV) *)

val upgrade_required : response
(** `426 Upgrade Required` *)

val precondition_required : response
(** `428 Precondition Required` *)

val too_many_requests : response
(** `429 Too Many Requests` *)

val request_header_fields_too_large : response
(** `431 Request Header Fields Too Large` *)

val blocked_by_windows_parental_controls : response
(** `450 Blocked by Windows Parental Controls` *)

val client_closed_request : response
(** `499 Client Closed Request` (nginx) *)

(** ## Server Error Responses (5xx) *)

val internal_server_error : response
(** `500 Internal Server Error` *)

val not_implemented : response
(** `501 Not Implemented` *)

val bad_gateway : response
(** `502 Bad Gateway` *)

val service_unavailable : response
(** `503 Service Unavailable` *)

val gateway_timeout : response
(** `504 Gateway Timeout` *)

val http_version_not_supported : response
(** `505 HTTP Version Not Supported` *)

val variant_also_negotiates : response
(** `506 Variant Also Negotiates` *)

val insufficient_storage : response
(** `507 Insufficient Storage` (WebDAV) *)

val loop_detected : response
(** `508 Loop Detected` (WebDAV) *)

val bandwidth_limit_exceeded : response
(** `509 Bandwidth Limit Exceeded` *)

val not_extended : response
(** `510 Not Extended` *)

val network_authentication_required : response
(** `511 Network Authentication Required` *)

(** ## Unofficial Status Codes *)

val checkpoint : response
(** `103 Checkpoint` *)

val processing : response
(** `102 Processing` *)

val continue : response
(** `100 Continue` *)

val switching_protocols : response
(** `101 Switching Protocols` *)

val no_response : response
(** `444 No Response` (nginx) *)

val retry_with : response
(** `449 Retry With` (Microsoft) *)

val wrong_exchange_server : response
(** `451 Unavailable For Legal Reasons` *)

val network_read_timeout_error : response
(** `598 Network read timeout error` *)

val network_connect_timeout_error : response
(** `599 Network connect timeout error` *)
