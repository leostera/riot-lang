(** {1 Type Environment}

    Tracks bindings (values, types, modules) during type checking.

    The environment is the "what's in scope" data structure. When type-checking
    an expression like [x + 1], we look up [x] in the environment to find its
    type.

    {b Design:} Purely functional style - adding bindings returns a new
    environment, original is unchanged. Uses hash maps internally for O(1)
    lookup. *)

(** {2 Environment Types} *)

type value_entry = {
  value_type : Types.type_expr;  (** The type of this value *)
  value_loc : Location.t option;
      (** Where this value was defined (for error messages) *)
}
(** Information about a value binding (variable or function). *)

type type_entry = {
  type_decl : Types.type_declaration;  (** The type declaration *)
  type_loc : Location.t option;  (** Where this type was defined *)
}
(** Information about a type definition. *)

type t
(** The type environment.

    Opaque type - use the provided functions to manipulate it. Internally uses
    hash maps for efficient lookup. *)

(** {2 Creating Environments} *)

val create : unit -> t
(** Create an empty environment.

    Use this at the start of type-checking a module, then add predefined types
    with {!add_predef_types}.

    Example:
    {[
      let env = create () in
      let env, ctx = add_predef_types env ~ctx in
      (* Ready to type-check! *)
    ]} *)

(** {2 Value Bindings} *)

val add_value :
  t -> Identifier.t -> Types.type_expr -> loc:Location.t option -> t
(** Add a value binding to the environment.

    Shadows any previous binding with the same name (OCaml allows shadowing).

    Example:
    {[
      let env = add_value env x_ident int_type ~loc:(Some loc) in
      (* Now x : int is in scope *)
    ]}

    @param env The current environment
    @param ident The identifier to bind
    @param ty The type of the value
    @param loc Where defined (for error messages)
    @return Updated environment (original unchanged) *)

val find_value : t -> Identifier.t -> value_entry option
(** Look up a value binding.

    Returns [None] if the identifier is not in scope.

    Example:
    {[
      match find_value env x_ident with
      | Some entry -> (* Found! entry.value_type is the type *)
      | None -> (* Unbound variable error *)
    ]}

    @param env The environment
    @param ident The identifier to find
    @return [Some entry] if found, [None] if not in scope *)

val find_value_type : t -> Identifier.t -> Types.type_expr option
(** Look up a value's type (convenience function).

    Equivalent to [find_value] but returns just the type.

    @param env The environment
    @param ident The identifier
    @return [Some type] if found, [None] if not in scope *)

(** {2 Type Definitions} *)

val add_type :
  t -> Identifier.t -> Types.type_declaration -> loc:Location.t option -> t
(** Add a type definition to the environment.

    Used when processing type declarations like:
    {[
      type point = { x : int; y : int }
    ]}

    @param env The environment
    @param ident The type name
    @param decl The type declaration
    @param loc Where defined
    @return Updated environment *)

val find_type : t -> Identifier.t -> type_entry option
(** Look up a type definition.

    @param env The environment
    @param ident The type name
    @return [Some entry] if found, [None] if undefined *)

val find_type_decl : t -> Identifier.t -> Types.type_declaration option
(** Look up a type declaration (convenience function).

    @param env The environment
    @param ident The type name
    @return [Some declaration] if found, [None] if undefined *)

(** {2 Predefined Types} *)

val add_predef_types : t -> ctx:Types.context -> t * Types.context
(** Add built-in types to the environment.

    Adds: [int], [string], [bool], [unit], [char], [float].

    Call this once when setting up the initial environment:
    {[
      let env = create () in
      let env, ctx = add_predef_types env ~ctx in
    ]}

    @param env The environment
    @param ctx The typing context
    @return Tuple of (updated environment, updated context) *)

(** {2 Type Constructors} *)

val type_int : ctx:Types.context -> Types.type_expr * Types.context
(** Create the [int] type.

    @param ctx Typing context
    @return Tuple of (int type, updated context) *)

val type_string : ctx:Types.context -> Types.type_expr * Types.context
(** Create the [string] type.

    @param ctx Typing context
    @return Tuple of (string type, updated context) *)

val type_bool : ctx:Types.context -> Types.type_expr * Types.context
(** Create the [bool] type.

    @param ctx Typing context
    @return Tuple of (bool type, updated context) *)

val type_unit : ctx:Types.context -> Types.type_expr * Types.context
(** Create the [unit] type.

    @param ctx Typing context
    @return Tuple of (unit type, updated context) *)

val type_arrow :
  ctx:Types.context ->
  Types.arg_label ->
  Types.type_expr ->
  Types.type_expr ->
  Types.type_expr * Types.context
(** Create a function type (arrow type).

    Example creating [int -> string]:
    {[
      let int_ty, ctx = type_int ~ctx in
      let string_ty, ctx = type_string ~ctx in
      let fn_ty, ctx = type_arrow ~ctx Nolabel int_ty string_ty in
    ]}

    @param ctx Typing context
    @param label Argument label ([Nolabel], [Labelled], or [Optional])
    @param arg_type The argument type
    @param ret_type The return type
    @return Tuple of (arrow type, updated context) *)

(** {2 Utility Functions} *)

val copy : t -> t
(** Create a shallow copy of the environment.

    Useful for speculative type-checking. Since we never mutate entries (only
    add new ones), shallow copy is safe and efficient.

    Example:
    {[
      let env_copy = copy env in
      (* Modify env_copy without affecting env *)
    ]}

    @param env The environment to copy
    @return A new environment with the same bindings *)
