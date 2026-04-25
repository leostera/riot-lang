(** {1 CORS Middleware}
    
    Simple Cross-Origin Resource Sharing (CORS) for Suri.
    
    Handles both preflight (OPTIONS) and simple CORS requests according to
    the W3C CORS specification.
    
    {2 Quick Examples}
    
    {3 Allow specific origins}
    {[
      let app = Middleware.[
        cors ~origins:["https://example.com"] ();
        router routes;
      ]
    ]}
    
    {3 Development (allow all)}
    {[
      let app = Middleware.[
        cors ~origins:["*"] ();
        router routes;
      ]
    ]}
    
    {3 With credentials}
    {[
      let app = Middleware.[
        cors 
          ~origins:["https://app.example.com"]
          ~credentials:true
          ();
        router routes;
      ]
    ]}
    
    {3 With custom headers and methods}
    {[
      let app = Middleware.[
        cors 
          ~origins:["https://api.example.com"]
          ~methods:[GET; POST; PUT; DELETE]
          ~headers:["authorization"; "content-type"]
          ~credentials:true
          ~max_age:86400
          ();
        router routes;
      ]
    ]}
    
    {3 Multiple specific origins}
    {[
      let app = Middleware.[
        cors ~origins:[
          "https://example.com";
          "https://app.example.com";
          "https://mobile.example.com";
        ] ();
        router routes;
      ]
    ]}
    
    {2 Security Notes}
    
    - {b Never} use wildcard ["*"] with [~credentials:true]
    - Be specific with [~headers] to avoid exposing sensitive data
    - Use [~max_age] to reduce preflight requests (3600-86400 seconds)
    - Test CORS configuration with browser DevTools
    
    {2 Origin Patterns}
    
    Origins can be specified as:
    - {b Exact match}: ["https://example.com"]
    - {b Wildcard}: ["*"] (allows any origin)
    
    {2 How CORS Works}
    
    {3 Preflight Requests (OPTIONS)}
    Browser sends OPTIONS request before actual request when:
    - Using non-simple methods (PUT, DELETE, PATCH, etc.)
    - Using custom headers (Authorization, X-Custom-Header, etc.)
    - Using Content-Type other than form-data/urlencoded/text-plain
    
    Middleware responds with:
    - [access-control-allow-origin] (the allowed origin)
    - [access-control-allow-methods] (allowed HTTP methods)
    - [access-control-allow-headers] (allowed custom headers)
    - [access-control-max-age] (how long to cache this response)
    
    {3 Simple Requests}
    For GET/HEAD/POST with simple headers, browser sends request directly.
    Middleware adds [access-control-allow-origin] to response.
    
    {2 Troubleshooting}
    
    {b Browser shows "CORS error"}
    - Check that origin is in [~origins] list
    - Verify middleware is {e before} router in pipeline
    - Check browser DevTools Network tab for preflight request
    - Enable debug logging: [Log.set_level Debug]
    
    {b Credentials not working}
    - Set [~credentials:true]
    - Cannot use ["*"] wildcard with credentials
    - Use exact origin or regex pattern instead
    
    {b Custom headers rejected}
    - Add header names to [~headers] parameter
    - Header names are case-insensitive
    - Check browser's preflight request in DevTools
*)
open Std

(** CORS middleware with simple configuration.
    
    @param origins List of allowed origins. Use ["*"] for all, or exact matches like ["https://example.com"].
    @param methods Allowed methods beyond simple ones (default: [PUT; PATCH; DELETE])
    @param headers Allowed custom headers (default: [] = only simple headers)
    @param credentials Allow credentials like cookies (default: false)
    @param expose Headers exposed to client JavaScript (default: [])
    @param max_age Preflight cache duration in seconds (default: none)
    
    {b Important}: Wildcard ["*"] with [~credentials:true] will log a warning
    as this is a security risk.
    
    {b Allowed by default}:
    - Methods: GET, HEAD, POST (always allowed for simple requests)
    - Headers: Accept, Accept-Language, Content-Language, Content-Type
    
    {b Example - Production API}:
    {[
      let app = Middleware.[
        request_id;
        logger;
        cors 
          ~origins:["https://app.production.com"]
          ~methods:[POST; PUT; PATCH; DELETE]
          ~headers:["authorization"; "content-type"]
          ~credentials:true
          ~max_age:86400  (* Cache for 24 hours *)
          ();
        router api_routes;
      ]
    ]}
    
    {b Example - Development}:
    {[
      let cors_config =
        if Env.get "APP_ENV" = Some "development" then
          cors ~origins:["*"] ()
        else
          cors ~origins:["https://app.production.com"] ~credentials:true ()
      in
      
      let app = Middleware.[
        request_id;
        logger;
        cors_config;
        router routes;
      ]
    ]} *)
val middleware:
  origins:string list ->
  ?methods:Net.Http.Method.t list ->
  ?headers:string list ->
  ?credentials:bool ->
  ?expose:string list ->
  ?max_age:int ->
  unit ->
  Pipeline.middleware
