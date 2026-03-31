(** Stdout handler - logs to standard output via a dedicated supervised process 
    
    Configuration is loaded from [log.stdout] config key:
    ```toml
    [log.stdout]
    format = "full"  # or "compact"
    ```
*)
val child_spec: unit -> Supervisor.child_spec

(** Get the supervisor child spec for the stdout handler.
    
    The handler loads its configuration from the [log.stdout] config section
    on startup. *)
val attach: unit -> unit

(** Attach the stdout handler callback.
    
    This is called automatically by the child_spec when the handler process starts. *)
val detach: unit -> unit

(** Detach the stdout handler callback *)
