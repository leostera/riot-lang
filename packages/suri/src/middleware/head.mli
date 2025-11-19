open Std

(** {1 HEAD Request Handler Middleware}

    Automatically handles HEAD requests by converting them to GET requests
    and stripping the response body.

    {b HTTP Compliance}: HTTP/1.1 requires servers to support HEAD requests.
    HEAD requests should return the same headers as GET but without the body.

    {2 Quick Start}

    {[
      let app = Middleware.[
        head;    (* Add this middleware *)
        logger;
        router routes;
      ]
    ]}

    {2 How It Works}

    1. Detects HEAD requests
    2. Internally processes as GET (routes work normally)
    3. Strips response body before sending
    4. Keeps all headers intact (Content-Length, ETag, etc.)

    {2 Why Use This?}

    - {b Browser preflight}: Browsers check if resources exist before downloading
    - {b Crawlers}: Search engines use HEAD to check resource freshness
    - {b API clients}: Check if endpoints exist without transferring data
    - {b HTTP compliance}: Required by HTTP/1.1 specification

    {2 Example}

    {[
      (* Without head middleware - need to handle HEAD explicitly *)
      let handler ~conn ~next:_ =
        match Conn.method_ conn with
        | Head -> Conn.respond conn ~status:Ok |> Conn.send
        | Get -> Conn.respond conn ~status:Ok ~body:"Content" |> Conn.send
        | _ -> Conn.respond conn ~status:MethodNotAllowed |> Conn.send

      (* With head middleware - just write GET handler! *)
      let handler ~conn ~next:_ =
        Conn.respond conn ~status:Ok ~body:"Content" |> Conn.send
        (* HEAD requests automatically handled *)
    ]}

    {2 Placement}

    Place early in the middleware stack, typically right after request_id:

    {[
      let app = Middleware.[
        request_id;
        head;      (* Early - before logging so GET is logged *)
        logger;
        router routes;
      ]
    ]} *)

(** {1 Middleware} *)

val middleware : conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t
(** HEAD request handler middleware.

    Automatically converts HEAD requests to GET for processing,
    then strips the response body before sending.

    This is a zero-configuration middleware - just add it to your pipeline.

    {[
      let app = Middleware.[
        head;
        router routes;
      ]
    ]}

    {b Behavior}:
    - HEAD requests → processed as GET → body stripped
    - All other requests → pass through unchanged
    - Headers preserved (Content-Length, ETag, etc.)

    {b Note}: This middleware must wrap around your handlers to strip
    the body after response is built. *)
