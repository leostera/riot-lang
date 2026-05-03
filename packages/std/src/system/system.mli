(**
   System information and operations.

   System-level operations and information queries with a small, explicit
   surface.

   ## See Also

   - [Env] for environment variables
   - [Command] for running external processes
*)
module TargetTriple = TargetTriple

module OS = Os

val host_triple: TargetTriple.t

val os_type: string

val unix: bool

val win32: bool

val cygwin: bool

(** Exit the current process immediately with the given status code. *)
val exit: int -> 'a
