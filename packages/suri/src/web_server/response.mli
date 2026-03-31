(** {1 HTTP Response Construction}

    Builder interface for HTTP responses with status codes, headers, and bodies.
    Provides convenient functions for all standard HTTP status codes.

    {2 Quick Reference}

    {3 Most Common}
    {[
      Response.ok ~body:"Success" ()                    (* 200 *)
      Response.created ~body:"Created" ()               (* 201 *)
      Response.redirect ~location:"/home" ()            (* 302 *)
      Response.bad_request ~body:"Invalid input" ()     (* 400 *)
      Response.unauthorized ~body:"Login required" ()   (* 401 *)
      Response.not_found ~body:"Not found" ()           (* 404 *)
      Response.internal_server_error ~body:"Error" ()   (* 500 *)
    ]}

    {3 With Headers}
    {[
      Response.ok
        ~headers:[
          ("Content-Type", "application/json");
          ("Cache-Control", "no-cache");
        ]
        ~body:{|{"status":"ok"}|}
        ()
    ]}

    {3 Custom Status}
    {[
      Response.make `IM_A_TEAPOT
        ~body:"I'm a teapot"
        ()
    ]}

    {2 Examples}

    {3 JSON Response}
    {[
      let json_response data =
        let json = Data.Json.to_string data in
        Response.ok
          ~headers:[("Content-Type", "application/json")]
          ~body:json
          ()
    ]}

    {3 Redirect}
    {[
      let handler _conn req =
        match Request.path req with
        | "/old-path" ->
            Response.moved_permanently
              ~headers:[("Location", "/new-path")]
              ()
        | _ ->
            Response.not_found ~body:"404" ()
    ]}

    {3 Error Handling}
    {[
      let handler _conn req =
        try
          let result = process_request req in
          Response.ok ~body:result ()
        with
        | Invalid_argument msg ->
            Response.bad_request ~body:("Invalid: " ^ msg) ()
        | Not_found ->
            Response.not_found ~body:"Resource not found" ()
        | _ ->
            Response.internal_server_error ~body:"Server error" ()
    ]}

    ---

    {1 API Reference} *)

open Std

(** HTTP response record.

    Contains status code, headers, HTTP version, and response body. *)
(** Create a custom HTTP response.

    Use this for non-standard status codes or when you need full control.
    For standard responses, use the convenience functions below.

    @param status HTTP status code (e.g., [`OK], [`Not_found])
    @param headers Optional list of (name, value) header pairs
    @param version HTTP version (default: [Http11])
    @param body Response body (default: empty string)

    Example:
    {[
      make `OK
        ~headers:[("Content-Type", "text/plain")]
        ~body:"Hello"
        ()
    ]} *)
type t = {
  status : Net.Http.Status.t;
  headers : Net.Http.Header.t;
  version : Net.Http.Version.t;
  body : string;
}
val make : Net.Http.Status.t ->
?headers:(string * string) list ->
?version:Net.Http.Version.t ->
?body:string ->
unit ->
t

(** Response builder function type.

    All convenience functions below follow this signature. *)
type response = ?headers:(string * string) list ->
?version:Net.Http.Version.t ->
?body:string ->
unit ->
t
(** {1 Success Responses (2xx)}

    Successful responses indicating the request was received and processed. *)
(** [200 OK] - Standard success response.

    Most common response for successful requests.

    Example:
    {[ ok ~body:"Success" () ]} *)
val ok : response

(** `200 OK` *)
val ok : response

(** [201 Created] - Resource successfully created.

    Use for POST requests that create new resources.

    Example:
    {[
      post "/users" (fun _conn req ->
        let user = create_user (Request.body req) in
        created
          ~headers:[("Location", "/users/" ^ user.id)]
          ~body:(user_to_json user)
          ())
    ]} *)
val created : response

(** [202 Accepted] - Request accepted for processing (async).

    Use when processing will happen asynchronously.

    Example:
    {[ accepted ~body:"Processing started" () ]} *)
val accepted : response

(** `203 Non-Authoritative Information` *)
val non_authoritative_information : response

(** [204 No Content] - Success with no response body.

    Common for DELETE requests or updates without return data.

    Example:
    {[
      delete "/users/:id" (fun _conn _req ->
        delete_user id;
        no_content ())
    ]} *)
val no_content : response

(** `205 Reset Content` *)
val reset_content : response

(** `206 Partial Content` *)
val partial_content : response

(** `207 Multi-Status` (WebDAV) *)
val multi_status : response

(** `208 Already Reported` (WebDAV) *)
val already_reported : response

(** `226 IM Used` *)
val im_used : response

(** {1 Redirection Responses (3xx)}

    Redirects indicating the client should take additional action. *)
(** [300 Multiple Choices] - Multiple redirect options available. *)
val multiple_choices : response

(** [301 Moved Permanently] - Resource permanently moved.

    Search engines update their indexes.

    Example:
    {[
      moved_permanently
        ~headers:[("Location", "/new-location")]
        ()
    ]} *)
val moved_permanently : response

(** [302 Found] - Temporary redirect.

    Most common redirect status. Also see {!see_other}.

    Example:
    {[
      found ~headers:[("Location", "/login")] ()
    ]} *)
val found : response

(** [303 See Other] - Redirect after POST.

    Use after successful POST to redirect to GET.

    Example:
    {[
      post "/users" (fun _conn req ->
        let user = create_user req in
        see_other
          ~headers:[("Location", "/users/" ^ user.id)]
          ())
    ]} *)
val see_other : response

(** `304 Not Modified` *)
val not_modified : response

(** `305 Use Proxy` *)
val use_proxy : response

(** `306 Switch Proxy` *)
val switch_proxy : response

(** `307 Temporary Redirect` *)
val temporary_redirect : response

(** `308 Permanent Redirect` *)
val permanent_redirect : response

(** {1 Client Error Responses (4xx)}

    Errors caused by invalid client requests. *)
(** [400 Bad Request] - Invalid request syntax or parameters.

    Use for validation errors or malformed requests.

    Example:
    {[
      match validate_input req with
      | Ok data -> ok ~body:(process data) ()
      | Error msg -> bad_request ~body:("Invalid: " ^ msg) ()
    ]} *)
val bad_request : response

(** [401 Unauthorized] - Authentication required.

    Use when user must log in.

    Example:
    {[
      match get_auth_token req with
      | None ->
          unauthorized
            ~headers:[("WWW-Authenticate", "Bearer")]
            ~body:"Login required"
            ()
      | Some token -> (* ... *)
    ]} *)
val unauthorized : response

(** [402 Payment Required] - Reserved for future use. *)
val payment_required : response

(** [403 Forbidden] - Authenticated but not authorized.

    User is logged in but doesn't have permission.

    Example:
    {[
      if not (has_permission user resource) then
        forbidden ~body:"Access denied" ()
      else
        ok ~body:resource ()
    ]} *)
val forbidden : response

(** [404 Not Found] - Resource doesn't exist.

    Most common error response.

    Example:
    {[
      match find_user id with
      | Some user -> ok ~body:(user_to_json user) ()
      | None -> not_found ~body:"User not found" ()
    ]} *)
val not_found : response

(** [405 Method Not Allowed] - HTTP method not supported.

    Include [Allow] header with supported methods.

    Example:
    {[
      method_not_allowed
        ~headers:[("Allow", "GET, POST")]
        ~body:"Method not allowed"
        ()
    ]} *)
val method_not_allowed : response

(** `406 Not Acceptable` *)
val not_acceptable : response

(** `407 Proxy Authentication Required` *)
val proxy_authentication_required : response

(** `408 Request Timeout` *)
val request_timeout : response

(** `409 Conflict` *)
val conflict : response

(** `410 Gone` *)
val gone : response

(** `411 Length Required` *)
val length_required : response

(** `412 Precondition Failed` *)
val precondition_failed : response

(** `413 Request Entity Too Large` *)
val request_entity_too_large : response

(** `414 Request-URI Too Long` *)
val request_uri_too_long : response

(** `415 Unsupported Media Type` *)
val unsupported_media_type : response

(** `416 Requested Range Not Satisfiable` *)
val requested_range_not_satisfiable : response

(** `417 Expectation Failed` *)
val expectation_failed : response

(** `418 I'm a teapot` *)
val im_a_teapot : response

(** `420 Enhance Your Calm` (Twitter) *)
val enhance_your_calm : response

(** `422 Unprocessable Entity` (WebDAV) *)
val unprocessable_entity : response

(** `423 Locked` (WebDAV) *)
val locked : response

(** `424 Failed Dependency` (WebDAV) *)
val failed_dependency : response

(** `426 Upgrade Required` *)
val upgrade_required : response

(** `428 Precondition Required` *)
val precondition_required : response

(** `429 Too Many Requests` *)
val too_many_requests : response

(** `431 Request Header Fields Too Large` *)
val request_header_fields_too_large : response

(** `450 Blocked by Windows Parental Controls` *)
val blocked_by_windows_parental_controls : response

(** `499 Client Closed Request` (nginx) *)
val client_closed_request : response

(** {1 Server Error Responses (5xx)}

    Errors caused by server failures. *)
(** [500 Internal Server Error] - Generic server error.

    Use when an unexpected error occurs.

    Example:
    {[
      try
        process_request req
      with exn ->
        Log.error "Request failed: %s" (Printexc.to_string exn);
        internal_server_error ~body:"Internal server error" ()
    ]} *)
val internal_server_error : response

(** [501 Not Implemented] - Feature not implemented.

    Example:
    {[
      not_implemented ~body:"This feature is coming soon" ()
    ]} *)
val not_implemented : response

(** [502 Bad Gateway] - Invalid response from upstream server. *)
val bad_gateway : response

(** [503 Service Unavailable] - Server temporarily unavailable.

    Use during maintenance or when overloaded.

    Example:
    {[
      if is_maintenance_mode () then
        service_unavailable
          ~headers:[("Retry-After", "3600")]
          ~body:"Under maintenance"
          ()
      else
        process_request req
    ]} *)
val service_unavailable : response

(** `504 Gateway Timeout` *)
val gateway_timeout : response

(** `505 HTTP Version Not Supported` *)
val http_version_not_supported : response

(** `506 Variant Also Negotiates` *)
val variant_also_negotiates : response

(** `507 Insufficient Storage` (WebDAV) *)
val insufficient_storage : response

(** `508 Loop Detected` (WebDAV) *)
val loop_detected : response

(** `509 Bandwidth Limit Exceeded` *)
val bandwidth_limit_exceeded : response

(** `510 Not Extended` *)
val not_extended : response

(** `511 Network Authentication Required` *)
val network_authentication_required : response

(** ## Unofficial Status Codes *)
(** `103 Checkpoint` *)
val checkpoint : response

(** `102 Processing` *)
val processing : response

(** `100 Continue` *)
val continue : response

(** `101 Switching Protocols` *)
val switching_protocols : response

(** `444 No Response` (nginx) *)
val no_response : response

(** `449 Retry With` (Microsoft) *)
val retry_with : response

(** `451 Unavailable For Legal Reasons` *)
val wrong_exchange_server : response

(** `598 Network read timeout error` *)
val network_read_timeout_error : response

(** `599 Network connect timeout error` *)
val network_connect_timeout_error : response
