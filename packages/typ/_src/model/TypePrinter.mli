open Std

(**
   Pretty-printers for prototype types and schemes.

   These are intentionally derived views over the structured type
   representation. Snapshot tests and diagnostics use them for compact display,
   but callers that need stable structure should keep the underlying
   [TypeRepr.t] or [TypeScheme.t]. 
*)
val type_to_string: TypeRepr.t -> string

val scheme_to_string: TypeScheme.t -> string
