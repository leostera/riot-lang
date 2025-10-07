(** # Log - Structured logging

    This module provides leveled logging with formatted output. Messages are
    filtered based on the current log level and output to stderr with timestamps
    and level indicators.

    ## Examples

    Basic logging:

    ```ocaml open Std

    (* Set log level *) Log.set_level Log.Debug;

    (* Log messages at different levels *) Log.info "Server started on port %d"
    8080; Log.debug "Request from %s" client_ip; Log.warn "Cache miss for key:
    %s" key; Log.error "Failed to connect: %s" error_msg;

    (* Trace is filtered out at Debug level *) Log.trace "Detailed trace info:
    %d" value; (* Won't print *) ```

    ## Log Levels

    From least to most severe: 1. `Trace` - Very detailed debugging information
    2. `Debug` - Debugging information 3. `Info` - Informational messages 4.
    `Warn` - Warning messages 5. `Error` - Error messages

    Only messages at or above the current level are printed.

    ## Output Format

    Messages are output to stderr with format: ``` [TIMESTAMP] [LEVEL] Message
    ``` *)

(** # Types *)

(** Log severity levels, from least to most severe *)
type level =
  | Trace  (** Most detailed debugging information *)
  | Debug  (** Debugging messages *)
  | Info  (** Informational messages (default) *)
  | Warn  (** Warning messages *)
  | Error  (** Error messages *)

(** # Configuration *)

val set_level : level -> unit
(** Sets the minimum log level.

    Only messages at or above this level will be printed. Default level is
    `Info`.

    ## Examples

    ```ocaml (* Enable all logging *) Log.set_level Log.Trace;

    (* Production setting *) Log.set_level Log.Warn;

    (* Debugging *) Log.set_level Log.Debug;

    (* Based on environment *) let level = match Sys.getenv_opt "LOG_LEVEL" with
    | Some "trace" -> Log.Trace | Some "debug" -> Log.Debug | Some "warn" ->
    Log.Warn | Some "error" -> Log.Error | _ -> Log.Info in Log.set_level level
    ``` *)

val get_level : unit -> level
(** Returns the current log level.

    ## Examples

    ```ocaml let current = Log.get_level () in Printf.printf "Current log level:
    %s\n" (match current with | Trace -> "TRACE" | Debug -> "DEBUG" | Info ->
    "INFO" | Warn -> "WARN" | Error -> "ERROR") ``` *)

val set_log_file : Path.t -> unit
(** Redirect log output to a file instead of stdout.

    Opens the file in append mode, creating it if it doesn't exist.

    ## Examples

    ```ocaml Log.set_log_file (Path.v "/tmp/myapp.log"); Log.info "This goes to
    the file"; ``` *)

(** # Logging Functions *)

val trace : ('a, unit, string, unit) format4 -> 'a
(** Logs a trace message (most detailed level).

    Use for very detailed debugging information that's usually too verbose for
    normal debugging.

    ## Examples

    ```ocaml Log.trace "Entering function with args: x=%d, y=%d" x y; Log.trace
    "Cache lookup for key: %s" key; Log.trace "SQL query: %s" query_string ```
*)

val debug : ('a, unit, string, unit) format4 -> 'a
(** Logs a debug message.

    Use for information useful during development and debugging.

    ## Examples

    ```ocaml Log.debug "Processing request: %s" request_id; Log.debug "Cache hit
    rate: %.2f%%" hit_rate; Log.debug "Connecting to %s:%d" host port ``` *)

val info : ('a, unit, string, unit) format4 -> 'a
(** Logs an informational message.

    Use for general informational messages about normal operation. This is the
    default level.

    ## Examples

    ```ocaml Log.info "Server listening on port %d" port; Log.info "Processing
    batch of %d items" count; Log.info "Database migration completed"; Log.info
    "User %s logged in" username ``` *)

val warn : ('a, unit, string, unit) format4 -> 'a
(** Logs a warning message.

    Use for potentially problematic situations that don't prevent operation but
    should be addressed.

    ## Examples

    ```ocaml Log.warn "Deprecated API endpoint called: %s" endpoint; Log.warn
    "High memory usage: %d MB" memory_mb; Log.warn "Retry attempt %d of %d"
    attempt max_retries; Log.warn "Slow query: %dms for %s" duration query ```
*)

val error : ('a, unit, string, unit) format4 -> 'a
(** Logs an error message.

    Use for error conditions that require attention but don't crash the
    application.

    ## Examples

    ```ocaml Log.error "Failed to connect to database: %s" error_msg; Log.error
    "Invalid configuration: %s" reason; Log.error "Unhandled exception in
    worker: %s" (Printexc.to_string exn); Log.error "Request failed with status
    %d: %s" status body ```

    ## Note

    Error logging doesn't terminate the program. For fatal errors, log the error
    then handle appropriately (retry, fallback, or exit). *)
