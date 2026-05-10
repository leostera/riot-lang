(**
   # Dotenv parser

   Parses dotenv source text into bindings without mutating the process
   environment.
*)

(**
   Parse dotenv source text.

   The parser may read the process environment when resolving `$NAME` and
   `${NAME}` substitutions. Bindings parsed earlier in the same source are also
   available to later substitutions.
*)
val parse: string -> (Types.binding list, Types.error) Std.Result.t
