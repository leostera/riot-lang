(** Environment operations for Kernel *)
val getenv : string -> string option

(** Get the value of an environment variable *)
val getenv_exn : string -> string

(** Get the value of an environment variable, raising Not_found if not set *)
val putenv : string -> string -> unit

(** Set an environment variable *)
val unsetenv : string -> unit

(** Remove an environment variable *)
val environment : unit -> string array

(** Return all environment variables as an array of "VAR=value" strings *)
val getcwd : unit -> string

(** Get the current working directory *)
val chdir : string -> unit

(** Change the current working directory *)
