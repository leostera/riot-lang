val getenv : string -> string option
val getenv_exn : string -> string
val putenv : string -> string -> unit
val unsetenv : string -> unit
val environment : unit -> string array
val getcwd : unit -> string
val chdir : string -> unit
