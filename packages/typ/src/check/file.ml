open Std
open Std.Collections

type t = {
  diagnostics: Diagnostics.Diagnostic.t list;
  bindings: Typing_context.value_binding list;
  typing_context: Typing_context.t;
}

let empty = { diagnostics = []; bindings = []; typing_context = Typing_context.empty }

let is_ok = fun file -> List.is_empty file.diagnostics

let serializer =
  Serde.Ser.record
    (
      Serde.Ser.fields
        [
          Serde.Ser.field "diagnostics" (Serde.Ser.contramap Array.from_list (Serde.Ser.array Diagnostics.Diagnostic.serializer))
            (
              fun (file: t) -> file.diagnostics
            );
          Serde.Ser.field "bindings" (Serde.Ser.contramap Array.from_list (Serde.Ser.array Typing_context.value_binding_serializer))
            (
              fun (file: t) -> file.bindings
            );
          Serde.Ser.field "typing_context" Typing_context.serializer
            (
              fun (file: t) -> file.typing_context
            );
        ]
    )
