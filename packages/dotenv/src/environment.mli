(**
   # Process environment application

   Applies parsed dotenv bindings to the process environment.
*)

(**
   Apply bindings and return the bindings that were actually written.

   Existing process environment variables are preserved by default. Pass
   `~on_existing:Types.OverwriteExisting` to replace existing values.
*)
val apply_collect: ?on_existing:Types.existing -> Types.binding list -> Types.binding list

(**
   Apply bindings to the process environment.

   This is the side-effecting variant used by the public `Dotenv.apply`
   function.
*)
val apply: ?on_existing:Types.existing -> Types.binding list -> unit
