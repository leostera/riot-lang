open Std

val command: Std.ArgParser.command

val run: Std.ArgParser.matches -> (unit, exn) result

val run_install_script:
  ?env:(string * string) list -> ?version:string -> script_path:Std.Path.t -> unit -> (unit, string) result
