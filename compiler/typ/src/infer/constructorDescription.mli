(**
   Constructor metadata used by expression and pattern inference.

   Variant constructors are stored in the environment as rich descriptions, not
   only as callable type schemes. The extra shape lets the checker handle
   constructor-specific syntax such as inline-record payloads.
*)

(** Hidden nominal type name for an inline-record constructor payload. *)
val inline_record_payload_ident: Ast.type_declaration -> Ast.type_constructor -> Ast.ident

(** Replace generalized variables in a constructor description with fresh vars. *)
val instantiate:
  State.t ->
  State.InferenceEnv.constructor_description ->
  State.InferenceEnv.constructor_description

(** Look up and instantiate a constructor from the current environment. *)
val instantiate_from_state:
  State.t ->
  Ast.ident ->
  State.InferenceEnv.constructor_description option

(** Build a constructor description while registering a type declaration. *)
val from_type_constructor:
  State.t ->
  Ast.type_declaration ->
  Ast.type_constructor ->
  State.InferenceEnv.constructor_description
