(** Visual Debugger Middleware
    
    Beautiful, detailed error pages for development - inspired by Phoenix and Laravel.
    
    Shows exception details, full stack traces with source code snippets,
    request/response inspection, and more.
    
    {b ⚠️ DEVELOPMENT ONLY} - Never use in production! This middleware:
    - Exposes source code
    - Shows internal paths and stack traces
    - May leak sensitive information
    
    ## How It Works
    
    The debugger middleware wraps your handlers and catches any exceptions:
    
    1. Catches the exception
    2. Captures the backtrace
    3. Logs the error to console
    4. Parses stack frames and extracts source code
    5. Renders a beautiful HTML error page
    6. Sends 500 response to the client
    
    The error is logged immediately so you see it in your terminal, then a
    beautiful HTML error page is sent to the browser for visual debugging.
    
    ## Features
    
    - 🔥 Beautiful error pages with dark theme
    - 📚 Full stack traces with syntax-highlighted source code
    - 📨 Complete request inspection (method, path, headers, params, body)
    - 📤 Response state inspection (status, headers before error)
    - 🎯 Pinpoints exact line where error occurred
    - 📝 Shows 5 lines of context around each frame
    - 📋 Logs errors to console
    - 🚀 Zero configuration - just add to middleware stack
    
    ## Usage
    
    Add debugger near the {b end} of your middleware stack (but before router):
    
    {[
      open Std
      open Suri
      
      let app = Middleware.[
        request_id;
        logger;      (* Logs successful requests *)
        debugger;    (* Catches exceptions, logs them, shows error page *)
        router routes;
      ]
      
      let () = Actors.run ~args:Env.args () ~main:(fun ~args:_ ->
        match Suri.start_link app with
        | Ok _ -> (* ... *)
        | Error _ -> (* ... *)
      )
    ]}
    
    ## Production Safety
    
    To disable in production:
    
    {[
      let is_development = Env.get "APP_ENV" = Some "development" in
      
      let app = Middleware.[
        request_id;
        logger;
        (* Only add debugger in development *)
      ] @ (if is_development then [Middleware.debugger] else []) @ [
        Middleware.router routes;
      ]
    ]}
    
    Or use a simpler pattern:
    
    {[
      let debug_middleware = match Env.get "APP_ENV" with
        | Some "production" -> []
        | _ -> [Middleware.debugger]
      in
      
      let app = Middleware.[request_id; logger] @ debug_middleware @ [router routes]
    ]}
    
    ## Example Error Page
    
    When an exception occurs, you'll see:
    
    {v
    ┌─────────────────────────────────────────────┐
    │ 🔥 500 Internal Server Error                │
    │                                             │
    │ Exception: Failure("User not found")        │
    ├─────────────────────────────────────────────┤
    │ 📚 Stack Trace                              │
    │                                             │
    │ ┌─────────────────────────────────────────┐ │
    │ │ handler.ml:42                           │ │
    │ │  40 | let find_user id =                │ │
    │ │  41 |   match DB.get id with            │ │
    │ │> 42 |   | None -> failwith "not found"  │ │ ← ERROR
    │ │  43 |   | Some u -> u                   │ │
    │ └─────────────────────────────────────────┘ │
    │                                             │
    │ 📨 Request: GET /users/123                  │
    │ 📤 Response: 200 OK (before error)          │
    └─────────────────────────────────────────────┘
    v}
    
    ## Technical Details
    
    {b Backtrace Parsing:}
    Uses [Printexc.get_backtrace()] to capture the stack trace, then parses
    OCaml's backtrace format to extract file paths, line numbers, and function names.
    
    {b Source Code Reading:}
    Reads source files from disk at request time using [Fs.read_to_string].
    Shows 5 lines of context before and after the error line.
    
    {b Workspace Source Resolution:}
    The debugger scans the local riot workspace to resolve sandbox paths back to
    actual source files. This provides clean workspace-relative paths like
    [packages/suri/src/handler.ml] instead of cryptic sandbox paths, and falls
    back to best-guess path construction if the workspace cannot be loaded.
    Resolved paths show a green checkmark (✓) badge in the error page.
    
    {b Error Logging:}
    The middleware logs errors to the console before sending the HTML page,
    so you see errors in your terminal as well as the browser.
    
    ## Limitations
    
    - Source code must be readable from disk at the reported paths
    - Only works with exceptions that have backtraces enabled
    - Cannot show source for compiled libraries without source available
    - Exceptions are caught and converted to 500 responses
    - Best results when the project is opened from the workspace root
    
    ## See Also
    
    - {!Logger} - Request/response logging (works with debugger via re-raise)
    - {!Component} - HTML component system used to render error pages
    - [Printexc] - OCaml exception and backtrace utilities
*)
val debugger: conn:Conn.t -> next:(Conn.t -> Conn.t) -> Conn.t

(** Debugger middleware that catches exceptions and displays detailed error pages.
    
    Catches exceptions, logs them to console, and sends beautiful HTML error pages.
    
    Example:
    {[
      let app = Middleware.[
        request_id;
        logger;     (* Logs successful requests *)
        debugger;   (* Catches & logs errors, shows error page *)
        router routes;
      ]
    ]}
    
    {b ⚠️ Development only} - do not use in production! *)
