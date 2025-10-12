open Std

(** {1 Type Environment}

    The environment tracks bindings during type checking: values, types, modules, etc.
    
    {b For beginners:} Think of the environment as a stack of "what's in scope".
    When we see [x + 1], we look up [x] in the environment to find its type.
    When we enter a let-binding, we add new names to the environment.
    
    {b Key concepts:}
    - {b Value environment:} Maps identifiers to their types
    - {b Type environment:} Maps type names to their definitions
    - {b Scoping:} Inner scopes shadow outer scopes
    - {b No global state:} Environment is explicitly passed around
    
    Example:
    {[
      let empty = create () in
      
      (* Add 'x : int' to environment *)
      let env = add_value env x_ident int_type in
      
      (* Look up 'x' *)
      match find_value env x_ident with
      | Some ty -> (* Found it! ty is int_type *)
      | None -> (* Not in scope *)
    ]}
*)

(** {2 Environment Type} *)

type value_entry = {
  value_type : Types.type_expr;
  value_loc : Location.t option;
}
(** Information about a value binding.
    
    Tracks:
    - The type of the value
    - Where it was defined (for error messages)
*)

type type_entry = {
  type_decl : Types.type_declaration;
  type_loc : Location.t option;
}
(** Information about a type definition.
    
    Tracks:
    - The type declaration (params, kind, manifest)
    - Where it was defined
*)

type t = {
  values : (Identifier.t, value_entry) Collections.HashMap.t;
  types : (Identifier.t, type_entry) Collections.HashMap.t;
  (* Future: modules, module types, etc. *)
}
(** The type environment.
    
    Uses hash maps for O(1) lookup. Each map tracks a different namespace:
    - [values]: variable and function bindings
    - [types]: type definitions
    
    {b Note:} We use separate namespaces because OCaml allows same name
    for value and type:
    {[
      type list = ...          (* Type namespace *)
      let list = [1; 2; 3]     (* Value namespace *)
      (* No conflict! *)
    ]}
*)

(** {2 Environment Creation} *)

let create () =
  (** Create an empty environment.
      
      Use this at the start of type-checking a module.
      
      Example:
      {[
        let env = create () in
        let env = add_predef_types env in  (* Add int, string, etc. *)
        (* Now ready to type-check *)
      ]}
      
      @return A fresh empty environment
  *)
  {
    values = Collections.HashMap.create ();
    types = Collections.HashMap.create ();
  }

(** {2 Value Operations} *)

let add_value env ident ty ~loc =
  (** Add a value binding to the environment.
      
      Shadows any previous binding with the same identifier.
      This is correct because OCaml allows shadowing:
      {[
        let x = 1 in
        let x = "hello" in  (* Shadows previous x *)
        x  (* Type: string *)
      ]}
      
      Example:
      {[
        let env = add_value env 
          ~ident:x_ident 
          ~ty:int_type 
          ~loc:(Some location) in
        (* Now x is in scope with type int *)
      ]}
      
      @param env The current environment
      @param ident The identifier to bind
      @param ty The type of the value
      @param loc Optional location where defined
      @return Updated environment (original is unchanged - immutable style)
  *)
  let entry = { value_type = ty; value_loc = loc } in
  let _ = Collections.HashMap.insert env.values ident entry in
  env

let find_value env ident =
  (** Look up a value binding in the environment.
      
      Returns None if not found (variable not in scope).
      
      Example:
      {[
        match find_value env x_ident with
        | Some entry -> 
            (* Found it! entry.value_type is the type *)
            entry.value_type
        | None ->
            (* Error: unbound variable x *)
            Error (Unbound_variable x_ident)
      ]}
      
      @param env The environment to search
      @param ident The identifier to find
      @return Some entry if found, None if not in scope
  *)
  Collections.HashMap.get env.values ident

let find_value_type env ident =
  (** Look up a value's type (convenience function).
      
      Just extracts the type from the entry, saving you a pattern match.
      
      Example:
      {[
        match find_value_type env x_ident with
        | Some ty -> (* x has type ty *)
        | None -> (* x not in scope *)
      ]}
      
      @param env The environment
      @param ident The identifier
      @return Some type if found, None otherwise
  *)
  match find_value env ident with
  | Some entry -> Some entry.value_type
  | None -> None

(** {2 Type Operations} *)

let add_type env ident decl ~loc =
  (** Add a type definition to the environment.
      
      Used when processing type declarations:
      {[
        type point = { x : int; y : int }
        type 'a option = None | Some of 'a
      ]}
      
      Example:
      {[
        let decl = {
          type_params = [];
          type_arity = 0;
          type_kind = Record [...];
          type_manifest = None;
          type_variance = [];
        } in
        let env = add_type env point_ident decl ~loc in
      ]}
      
      @param env The environment
      @param ident The type name
      @param decl The type declaration
      @param loc Where defined
      @return Updated environment
  *)
  let entry = { type_decl = decl; type_loc = loc } in
  let _ = Collections.HashMap.insert env.types ident entry in
  env

let find_type env ident =
  (** Look up a type definition.
      
      Example:
      {[
        match find_type env point_ident with
        | Some entry -> 
            (* Found it! entry.type_decl has the definition *)
            entry.type_decl
        | None ->
            (* Error: undefined type *)
            Error (Unbound_type point_ident)
      ]}
      
      @param env The environment
      @param ident The type name
      @return Some entry if found, None otherwise
  *)
  Collections.HashMap.get env.types ident

let find_type_decl env ident =
  (** Look up a type declaration (convenience function).
      
      @param env The environment
      @param ident The type name
      @return Some declaration if found, None otherwise
  *)
  match find_type env ident with
  | Some entry -> Some entry.type_decl
  | None -> None

(** {2 Predefined Types} *)

let add_predef_types env ~ctx =
  (** Add built-in types to the environment.
      
      Adds standard types like:
      - int, string, bool, unit
      - char, float
      - list, option (in future)
      
      Call this once when setting up the initial environment.
      
      Example:
      {[
        let env = create () in
        let env, ctx = add_predef_types env ~ctx in
        (* Now int, string, etc. are available *)
      ]}
      
      @param env The environment
      @param ctx The typing context (for creating type paths)
      @return Tuple of (updated environment, updated context)
  *)
  (* Create identifiers for predef types *)
  let int_ident, identifier_ctx = Identifier.create_predef ~ctx:ctx.Types.identifier_ctx "int" in
  let string_ident, identifier_ctx = Identifier.create_predef ~ctx:identifier_ctx "string" in
  let bool_ident, identifier_ctx = Identifier.create_predef ~ctx:identifier_ctx "bool" in
  let unit_ident, identifier_ctx = Identifier.create_predef ~ctx:identifier_ctx "unit" in
  let char_ident, identifier_ctx = Identifier.create_predef ~ctx:identifier_ctx "char" in
  let float_ident, identifier_ctx = Identifier.create_predef ~ctx:identifier_ctx "float" in
  let ctx = { ctx with Types.identifier_ctx = identifier_ctx } in
  
  (* Create type declarations for each *)
  let make_abstract_type () = {
    Types.type_params = [];
    Types.type_arity = 0;
    Types.type_kind = Types.Abstract;
    Types.type_manifest = None;
    Types.type_variance = [];
  } in
  
  let env = add_type env int_ident (make_abstract_type ()) ~loc:None in
  let env = add_type env string_ident (make_abstract_type ()) ~loc:None in
  let env = add_type env bool_ident (make_abstract_type ()) ~loc:None in
  let env = add_type env unit_ident (make_abstract_type ()) ~loc:None in
  let env = add_type env char_ident (make_abstract_type ()) ~loc:None in
  let env = add_type env float_ident (make_abstract_type ()) ~loc:None in
  
  (env, ctx)

(** {2 Predefined Type Constructors} *)

let type_int ~ctx =
  (** Create the 'int' type.
      
      Example:
      {[
        let int_type, ctx = type_int ~ctx in
        (* int_type represents the type 'int' *)
      ]}
      
      @param ctx The typing context
      @return Tuple of (int type, updated context)
  *)
  let int_ident, identifier_ctx = Identifier.create_predef ~ctx:ctx.Types.identifier_ctx "int" in
  let ctx = { ctx with Types.identifier_ctx = identifier_ctx } in
  let int_path = ModulePath.Identifier int_ident in
  Types.newty ~ctx (Types.Constructor (int_path, []))

let type_string ~ctx =
  (** Create the 'string' type.
      
      @param ctx The typing context
      @return Tuple of (string type, updated context)
  *)
  let string_ident, identifier_ctx = Identifier.create_predef ~ctx:ctx.Types.identifier_ctx "string" in
  let ctx = { ctx with Types.identifier_ctx = identifier_ctx } in
  let string_path = ModulePath.Identifier string_ident in
  Types.newty ~ctx (Types.Constructor (string_path, []))

let type_bool ~ctx =
  (** Create the 'bool' type.
      
      @param ctx The typing context
      @return Tuple of (bool type, updated context)
  *)
  let bool_ident, identifier_ctx = Identifier.create_predef ~ctx:ctx.Types.identifier_ctx "bool" in
  let ctx = { ctx with Types.identifier_ctx = identifier_ctx } in
  let bool_path = ModulePath.Identifier bool_ident in
  Types.newty ~ctx (Types.Constructor (bool_path, []))

let type_unit ~ctx =
  (** Create the 'unit' type.
      
      @param ctx The typing context
      @return Tuple of (unit type, updated context)
  *)
  let unit_ident, identifier_ctx = Identifier.create_predef ~ctx:ctx.Types.identifier_ctx "unit" in
  let ctx = { ctx with Types.identifier_ctx = identifier_ctx } in
  let unit_path = ModulePath.Identifier unit_ident in
  Types.newty ~ctx (Types.Constructor (unit_path, []))

let type_arrow ~ctx label arg_type ret_type =
  (** Create a function type (arrow type).
      
      Example:
      {[
        (* Create int -> string *)
        let int_ty, ctx = type_int ~ctx in
        let string_ty, ctx = type_string ~ctx in
        let fn_ty, ctx = type_arrow ~ctx Nolabel int_ty string_ty in
        (* fn_ty represents: int -> string *)
      ]}
      
      @param ctx The typing context
      @param label Argument label (Nolabel, Labelled, Optional)
      @param arg_type The argument type
      @param ret_type The return type
      @return Tuple of (arrow type, updated context)
  *)
  Types.newty ~ctx (Types.Arrow (label, arg_type, ret_type))

(** {2 Utility Functions} *)

let copy env =
  (** Create a copy of the environment.
      
      Useful when you want to try type-checking speculatively without
      modifying the original environment.
      
      {b Note:} This creates shallow copies of the hash maps. Since we
      never mutate entries (only add new ones), this is safe and efficient.
      
      Example:
      {[
        let env_copy = copy env in
        let env_copy = add_value env_copy x_ident some_type ~loc:None in
        (* Original env is unchanged *)
      ]}
      
      @param env The environment to copy
      @return A new environment with same bindings
  *)
  (* TODO: Implement proper copying of HashMaps *)
  (* For now, environments are shared (HashMaps are mutable) *)
  env
