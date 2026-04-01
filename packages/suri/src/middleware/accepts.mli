open Std

(** {1 Content Negotiation Middleware}

    Validate request Accept and Content-Type headers to ensure APIs only
    process requests in supported formats.

    {b Quick Start}

    JSON-only API:
    {[
      let app = Middleware.[
        logger;
        accepts ["application/json"];
        body_parser ();
        router routes;
      ]
    ]}

    Multi-format API:
    {[
      let app = Middleware.[
        logger;
        accepts ["application/json"; "application/xml"; "text/csv"];
        router routes;
      ]
    ]}

    {2 How It Works}

    The middleware checks two things:
    
    1. {b Accept Header} (GET, POST, PUT, DELETE, etc.): What formats the client
       can handle in the response. Returns 406 Not Acceptable if no match.
    
    2. {b Content-Type Header} (POST, PUT, PATCH only): What format the client
       is sending in the request body. Returns 415 Unsupported Media Type if no match.

    {2 Wildcards}

    Supports standard MIME type wildcards:
    - [*/*] - Accept any content type
    - [text/*] - Accept any text type ([text/plain], [text/html], etc.)
    - [image/*] - Accept any image type

    {2 Quality Values}

    Parses quality values (q=0.8) from Accept headers:
    {[
      Accept: application/json;q=0.8, text/html, */*;q=0.1
    ]}
    
    Checks matches in order of quality (highest first).

    {2 Middleware Order}

    Place {b before} body parsing to avoid wasting time parsing unsupported content:
    {[
      let app = Middleware.[
        logger;
        accepts ["application/json"];  (* Check first *)
        body_parser ();                (* Only parse if accepted *)
        router routes;
      ]
    ]}

    {2 Examples}

    With custom error response:
    {[
      let config = Accepts.{
        types = ["application/json"];
        check_accept = true;
        check_content_type = true;
        on_reject = Some (fun conn received ->
          let body = Json.object_ [
            ("error", Json.string "Unsupported media type");
            ("expected", Json.string "application/json");
            ("received", Json.string (Option.value received ~default:"none"));
          ] |> Json.to_string in
          Conn.respond conn ~status:UnsupportedMediaType ~body
          |> Conn.with_header "Content-Type" "application/json"
          |> Conn.halt
        );
      } in
      
      let app = Middleware.[
        logger;
        accepts ~config ();
        router routes;
      ]
    ]} *)

(** {1 Types} *)

type config = {
  types: string list;
  (** List of accepted MIME types. Use wildcards like ["text/*"] or ["*/*"].
          Examples: [["application/json"]; ["text/html"; "text/plain"]; ["*/*"]] *)
  check_accept: bool;
  (** Check Accept header on all requests. Default: true.
          When false, only Content-Type is checked (for POST/PUT/PATCH). *)
  check_content_type: bool;
  (** Check Content-Type on POST/PUT/PATCH requests. Default: true.
          When false, only Accept header is checked. *)
  on_reject: (Conn.t -> string option -> Conn.t) option;
  (** Custom rejection handler. Receives the connection and the received
          header value (Accept or Content-Type that didn't match).
          Should return a halted connection with appropriate error response.
          Default: returns simple 406/415 with plain text body. *)
}
val default_config: config

(** Default configuration:
    - types: [["*/*"]] (accept all)
    - check_accept: true
    - check_content_type: true
    - on_reject: None (use built-in 406/415 responses) *)
(** {1 Middleware} *)

val middleware: ?config:config -> string list -> Pipeline.middleware

(** Create content negotiation middleware.

    {[
      (* Simple usage *)
      accepts ["application/json"]
      
      (* Multiple types *)
      accepts ["application/json"; "text/plain"; "text/html"]
      
      (* Wildcards *)
      accepts ["text/*"; "application/json"]
      
      (* Custom config *)
      accepts ~config:{ default_config with check_accept = false } 
        ["application/json"]
    ]}

    @param config Optional configuration (uses {!default_config} if not provided)
    @param types List of accepted MIME types (shorthand for config.types)

    Returns middleware that:
    - Checks Accept header matches one of the types
    - Checks Content-Type header (for POST/PUT/PATCH) matches one of the types
    - Returns 406 Not Acceptable if Accept doesn't match
    - Returns 415 Unsupported Media Type if Content-Type doesn't match
    - Calls next middleware if both checks pass

    {b Order matters}: Place before body parsing middleware!
    
    {[
      (* CORRECT *)
      let app = Middleware.[
        accepts ["application/json"];
        body_parser ();
        router routes;
      ]
      
      (* WRONG - wastes time parsing before checking type *)
      let app = Middleware.[
        body_parser ();
        accepts ["application/json"];
        router routes;
      ]
    ]} *)
val make: config -> Pipeline.middleware

(** Create middleware with full configuration.

    {[
      let config = Accepts.{
        types = ["application/json"; "application/xml"];
        check_accept = true;
        check_content_type = true;
        on_reject = Some (fun conn received ->
          (* Custom JSON error response *)
          let body = Printf.sprintf 
            {|{"error": "Unsupported media type", "received": "%s"}|}
            (Option.value received ~default:"none")
          in
          Conn.respond conn ~status:UnsupportedMediaType ~body
          |> Conn.with_header "Content-Type" "application/json"
          |> Conn.halt
        );
      } in
      
      accepts ~config:config []
    ]}

    @param config Full configuration object *)
(** {1 Helper Functions} *)

val matches_pattern: pattern:string -> content_type:string -> bool

(** Check if a content type matches a pattern.

    Supports:
    - Exact match: ["application/json"] matches ["application/json"]
    - Type wildcard: ["text/*"] matches ["text/plain"], ["text/html"], etc.
    - Full wildcard: ["*/*"] matches anything

    {[
      matches_pattern ~pattern:"application/json" ~content_type:"application/json"
      (* true *)
      
      matches_pattern ~pattern:"text/*" ~content_type:"text/plain"
      (* true *)
      
      matches_pattern ~pattern:"*/*" ~content_type:"image/png"
      (* true *)
      
      matches_pattern ~pattern:"application/json" ~content_type:"text/plain"
      (* false *)
    ]} *)
type accept_entry = {
  media_type: string;
  quality: float;
}

(** Entry in parsed Accept header.
    - media_type: MIME type (e.g., "application/json")
    - quality: Quality value from 0.0 to 1.0 (default: 1.0) *)
val parse_accept: string -> accept_entry list

(** Parse Accept header with quality values.

    Returns list sorted by quality (highest first).

    {[
      parse_accept "application/json"
      (* [{ media_type = "application/json"; quality = 1.0 }] *)
      
      parse_accept "text/html, application/json;q=0.8, */*;q=0.1"
      (* [
        { media_type = "text/html"; quality = 1.0 };
        { media_type = "application/json"; quality = 0.8 };
        { media_type = "*/*"; quality = 0.1 };
      ] *)
    ]} *)
val get_base_content_type: string -> string option

(** Extract base content type from Content-Type header.

    Strips parameters like charset, boundary, etc.

    {[
      get_base_content_type "application/json"
      (* Some "application/json" *)
      
      get_base_content_type "application/json; charset=utf-8"
      (* Some "application/json" *)
      
      get_base_content_type "multipart/form-data; boundary=----WebKit..."
      (* Some "multipart/form-data" *)
      
      get_base_content_type ""
      (* None *)
    ]} *)
