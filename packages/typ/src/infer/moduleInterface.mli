(**
   `ModuleInterface.t` is the small summary produced by the new one-shot
   inference path. It should eventually be the thing we render as an inferred
   signature and persist in the content store for dependency reuse.

   The current slice records exported types and values by identifier.
*)
type t

(**
   Project the final inference environment into an exported module interface.

   This currently aliases the environment's value map as a temporary shortcut.
   Once interfaces are cached or retained beyond a single check, this should
   copy the visible exports instead.
*)
val from_env: Env.t -> t

(**
   Iterate over exported values.

   The iterator order comes from the inference environment, so the signature
   renderer can stay a pure formatter over already-ordered exports.
*)
val values: t -> (Ast.ident * TypeScheme.t) Std.Iter.Iterator.t

(**
   Iterate over exported type declarations.

   The iterator order comes from the inference environment, so type
   declarations are rendered in source addition order.
*)
val types: t -> (Ast.ident * Ast.type_declaration) Std.Iter.Iterator.t

(**
   Iterate over nested modules exported directly by this interface.

   Nested modules are copied recursively from the inference environment so the
   interface can be rendered or cached without retaining mutable state.
*)
val modules: t -> (Ast.ident * t) Std.Iter.Iterator.t
