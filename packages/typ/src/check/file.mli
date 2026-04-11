type t = {
  diagnostics: Diagnostics.Diagnostic.t list;
  bindings: Typing_context.value_binding list;
  typing_context: Typing_context.t;
}

val empty : t

val is_ok : t -> bool

val serializer : t Serde.Ser.t
