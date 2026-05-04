(**
   HTTP response builders for status codes, headers, versions, and bodies.

   ```ocaml
   Response.ok ~body:"Success" ()
   Response.created ~body:"Created" ()
   Response.bad_request ~body:"Invalid input" ()
   Response.internal_server_error ~body:"Error" ()
   ```

   ```ocaml
   Response.ok
     ~headers:[
       ("Content-Type", "application/json");
       ("Cache-Control", "no-cache");
     ]
     ~body:{json|{"status":"ok"}|json}
     ()
   ```

   ```ocaml
   let handler _conn req =
     match Request.path req with
     | "/old-path" ->
         Response.moved_permanently ~headers:[ ("Location", "/new-path") ] ()
     | _ ->
         Response.not_found ~body:"404" ()
   ```
*)
open Std

(**
   HTTP response record.

   Contains status code, headers, HTTP version, and response body.
*)
type t = {
  status: Net.Http.Status.t;
  headers: Net.Http.Header.t;
  version: Net.Http.Version.t;
  body: string;
}

(**
   Create a custom HTTP response.

   Use this for non-standard status codes or when you need full control.
   For standard responses, use the convenience functions below.

   ```ocaml
   make
     Net.Http.Status.Ok
     ~headers:[ ("Content-Type", "text/plain") ]
     ~body:"Hello"
     ()
   ```
*)
val make:
  Net.Http.Status.t ->
  ?headers:(string * string) list ->
  ?version:Net.Http.Version.t ->
  ?body:string ->
  unit ->
  t

(**
   Response builder function type.

   All convenience functions below follow this signature.
*)
type response =
  ?headers:(string * string) list ->
  ?version:Net.Http.Version.t ->
  ?body:string ->
  unit ->
  t

(**
   [200 OK] - Standard success response.

   Most common response for successful requests.

   ```ocaml
   ok ~body:"Success" ()
   ```
*)
val ok: response

(**
   [201 Created] - Resource successfully created.

   Use for POST requests that create new resources.

   ```ocaml
   post "/users" (fun _conn req ->
     let user = create_user (Request.body req) in
     created
       ~headers:[ ("Location", "/users/" ^ user.id) ]
       ~body:(user_to_json user)
       ())
   ```
*)
val created: response

(**
   [202 Accepted] - Request accepted for processing (async).

   Use when processing will happen asynchronously.

   ```ocaml
   accepted ~body:"Processing started" ()
   ```
*)
val accepted: response

(** `203 Non-Authoritative Information` *)
val non_authoritative_information: response

(**
   [204 No Content] - Success with no response body.

   Common for DELETE requests or updates without return data.

   ```ocaml
   delete "/users/:id" (fun _conn _req ->
     delete_user id;
     no_content ())
   ```
*)
val no_content: response

(** `205 Reset Content` *)
val reset_content: response

(** `206 Partial Content` *)
val partial_content: response

(** `207 Multi-Status` (WebDAV) *)
val multi_status: response

(** `208 Already Reported` (WebDAV) *)
val already_reported: response

(** `226 IM Used` *)
val im_used: response

(** [300 Multiple Choices] - Multiple redirect options available. *)
val multiple_choices: response

(**
   [301 Moved Permanently] - Resource permanently moved.

   Search engines update their indexes.

   ```ocaml
   moved_permanently
     ~headers:[ ("Location", "/new-location") ]
     ()
   ```
*)
val moved_permanently: response

(**
   [302 Found] - Temporary redirect.

   Most common redirect status. Also see `see_other`.

   ```ocaml
   found ~headers:[ ("Location", "/login") ] ()
   ```
*)
val found: response

(**
   [303 See Other] - Redirect after POST.

   Use after successful POST to redirect to GET.

   ```ocaml
   post "/users" (fun _conn req ->
     let user = create_user req in
     see_other
       ~headers:[ ("Location", "/users/" ^ user.id) ]
       ())
   ```
*)
val see_other: response

(** `304 Not Modified` *)
val not_modified: response

(** `305 Use Proxy` *)
val use_proxy: response

(** `306 Switch Proxy` *)
val switch_proxy: response

(** `307 Temporary Redirect` *)
val temporary_redirect: response

(** `308 Permanent Redirect` *)
val permanent_redirect: response

(**
   [400 Bad Request] - Invalid request syntax or parameters.

   Use for validation errors or malformed requests.

   ```ocaml
   match validate_input req with
   | Ok data -> ok ~body:(process data) ()
   | Error msg -> bad_request ~body:("Invalid: " ^ msg) ()
   ```
*)
val bad_request: response

(**
   [401 Unauthorized] - Authentication required.

   Use when user must log in.

   ```ocaml
   match get_auth_token req with
   | None ->
       unauthorized
         ~headers:[ ("WWW-Authenticate", "Bearer") ]
         ~body:"Login required"
         ()
   | Some token ->
       use_token token
   ```
*)
val unauthorized: response

(** [402 Payment Required] - Reserved for future use. *)
val payment_required: response

(**
   [403 Forbidden] - Authenticated but not authorized.

   User is logged in but doesn't have permission.

   ```ocaml
   if not (has_permission user resource) then
     forbidden ~body:"Access denied" ()
   else
     ok ~body:resource ()
   ```
*)
val forbidden: response

(**
   [404 Not Found] - Resource doesn't exist.

   Most common error response.

   ```ocaml
   match find_user id with
   | Some user -> ok ~body:(user_to_json user) ()
   | None -> not_found ~body:"User not found" ()
   ```
*)
val not_found: response

(**
   [405 Method Not Allowed] - HTTP method not supported.

   Include [Allow] header with supported methods.

   ```ocaml
   method_not_allowed
     ~headers:[ ("Allow", "GET, POST") ]
     ~body:"Method not allowed"
     ()
   ```
*)
val method_not_allowed: response

(** `406 Not Acceptable` *)
val not_acceptable: response

(** `407 Proxy Authentication Required` *)
val proxy_authentication_required: response

(** `408 Request Timeout` *)
val request_timeout: response

(** `409 Conflict` *)
val conflict: response

(** `410 Gone` *)
val gone: response

(** `411 Length Required` *)
val length_required: response

(** `412 Precondition Failed` *)
val precondition_failed: response

(** `413 Request Entity Too Large` *)
val request_entity_too_large: response

(** `414 Request-URI Too Long` *)
val request_uri_too_long: response

(** `415 Unsupported Media Type` *)
val unsupported_media_type: response

(** `416 Requested Range Not Satisfiable` *)
val requested_range_not_satisfiable: response

(** `417 Expectation Failed` *)
val expectation_failed: response

(** `418 I'm a teapot` *)
val im_a_teapot: response

(** `420 Enhance Your Calm` (Twitter) *)
val enhance_your_calm: response

(** `422 Unprocessable Entity` (WebDAV) *)
val unprocessable_entity: response

(** `423 Locked` (WebDAV) *)
val locked: response

(** `424 Failed Dependency` (WebDAV) *)
val failed_dependency: response

(** `426 Upgrade Required` *)
val upgrade_required: response

(** `428 Precondition Required` *)
val precondition_required: response

(** `429 Too Many Requests` *)
val too_many_requests: response

(** `431 Request Header Fields Too Large` *)
val request_header_fields_too_large: response

(** `450 Blocked by Windows Parental Controls` *)
val blocked_by_windows_parental_controls: response

(** `499 Client Closed Request` (nginx) *)
val client_closed_request: response

(**
   [500 Internal Server Error] - Generic server error.

   Use when an unexpected error occurs.

   ```ocaml
   try
     process_request req
   with exn ->
     Log.error (Exception.to_string exn);
     internal_server_error ~body:"Internal server error" ()
   ```
*)
val internal_server_error: response

(**
   [501 Not Implemented] - Feature not implemented.

   ```ocaml
   not_implemented ~body:"This feature is coming soon" ()
   ```
*)
val not_implemented: response

(** [502 Bad Gateway] - Invalid response from upstream server. *)
val bad_gateway: response

(**
   [503 Service Unavailable] - Server temporarily unavailable.

   Use during maintenance or when overloaded.

   ```ocaml
   if is_maintenance_mode () then
     service_unavailable
       ~headers:[ ("Retry-After", "3600") ]
       ~body:"Under maintenance"
       ()
   else
     process_request req
   ```
*)
val service_unavailable: response

(** `504 Gateway Timeout` *)
val gateway_timeout: response

(** `505 HTTP Version Not Supported` *)
val http_version_not_supported: response

(** `506 Variant Also Negotiates` *)
val variant_also_negotiates: response

(** `507 Insufficient Storage` (WebDAV) *)
val insufficient_storage: response

(** `508 Loop Detected` (WebDAV) *)
val loop_detected: response

(** `509 Bandwidth Limit Exceeded` *)
val bandwidth_limit_exceeded: response

(** `510 Not Extended` *)
val not_extended: response

(** `511 Network Authentication Required` *)
val network_authentication_required: response

(** `103 Checkpoint` *)
val checkpoint: response

(** `102 Processing` *)
val processing: response

(** `100 Continue` *)
val continue: response

(** `101 Switching Protocols` *)
val switching_protocols: response

(** `444 No Response` (nginx) *)
val no_response: response

(** `449 Retry With` (Microsoft) *)
val retry_with: response

(** `451 Unavailable For Legal Reasons` *)
val wrong_exchange_server: response

(** `598 Network read timeout error` *)
val network_read_timeout_error: response

(** `599 Network connect timeout error` *)
val network_connect_timeout_error: response
