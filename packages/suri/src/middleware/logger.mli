(** Request/response logging middleware.
    
    Automatically logs all HTTP requests with:
    - Request method and path
    - Response status code
    - Request duration in milliseconds
    
    Log levels are automatic:
    - 5xx responses → Error
    - 4xx responses → Warn
    - Slow requests (>1s) → Warn
    - Normal requests → Info
    
    Example:
    {[
      let app = Middleware.[
        logger;
        router routes;
      ]
    ]}
    
    Log output format:
    {v
    2025-01-15 10:30:45 | INFO | GET / -> 200 in 1ms
    2025-01-15 10:30:50 | WARN | GET /slow -> 200 in 1250ms
    2025-01-15 10:30:52 | WARN | GET /missing -> 404 in 0ms
    2025-01-15 10:30:55 | ERROR | GET /error -> 500 in 5ms
    v} *)

open Std

val logger : Pipeline.middleware
(** Request logger middleware.
    
    Logs format: [METHOD /path -> STATUS in DURATIONms]
    
    Example:
    {v
    GET / -> 200 in 1ms
    GET /api/users -> 200 in 5ms
    POST /api/login -> 401 in 2ms
    v} *)
