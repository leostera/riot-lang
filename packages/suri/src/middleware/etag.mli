open Std

(** {1 ETag Middleware}

    Automatically generates ETag headers for responses based on content hash.

    {2 Quick Start}

    {[
      let app = Middleware.[
        logger;
        conditional_get;  (* Check ETag before *)
        etag;             (* Generate ETag after *)
        router routes;
      ]
    ]}

    {2 What is an ETag?}

    An ETag (Entity Tag) is a unique identifier for a specific version of content:
    {v
    HTTP/1.1 200 OK
    ETag: "686897696a7c876b7e"
    Content-Type: text/html

    <html>...</html>
    v}

    Clients can cache and revalidate:
    {v
    GET /page HTTP/1.1
    If-None-Match: "686897696a7c876b7e"

    HTTP/1.1 304 Not Modified  (no body sent!)
    v}

    {2 Benefits}

    - ✅ Reduces bandwidth (304 responses have no body)
    - ✅ Faster page loads (client uses cached version)
    - ✅ Works with CDNs and proxies
    - ✅ Automatic - no manual cache keys needed

    {2 Strong vs Weak ETags}

    {b Strong ETag}: Byte-for-byte identical content
    {v ETag: "686897696a7c876b7e" v}

    {b Weak ETag}: Semantically equivalent (minor differences OK)
    {v ETag: W/"686897696a7c876b7e" v}

    {[
      (* Strong ETags (default) *)
      etag;

      (* Weak ETags (faster, less strict) *)
      etag ~weak:true;
    ]} *)

(** {1 Middleware} *)

val middleware : ?weak:bool -> conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
(** ETag generation middleware.

    Generates ETags from response body hash.

    {[
      let app = Middleware.[
        etag;
        router routes;
      ]
    ]}

    {b Parameters}:
    - [weak] - Generate weak ETags (default: false)

    {b Behavior}:
    - Hashes response body
    - Adds ETag header
    - Skips if body is empty
    - Skips if ETag already set

    {b Note}: Use with conditional_get middleware for automatic 304 responses. *)
