(** Internal numeric bridge for native-facing code paths. Public callers should
    stay on [System_error.t]. *)
val of_system_error: System_error.t -> int

val broken_pipe: int

val no_such_file_or_directory: int
