open Std

(**
   Planner-owned dependency summaries.

   The parallel phase shrinks a parsed Syn Ast into a tiny dependency IR. The
   serial phase replays that IR against package/module providers to produce the
   module roots a source depends on.
*)
module Item: sig
  module Ident: sig
    type t

    val of_strings: string list -> t

    val to_strings: t -> string list

    val length: t -> int
  end

  type include_mode =
    | Structure
    | Signature
  type functor_arg = {
    name: string option;
    ascription: t list;
  }

  and bound_module = {
    name: string;
    ascription: t list;
  }

  and t =
    | Use of Ident.t
    | Open of t
    | ImplicitOpen of t
    | Include of include_mode * t
    | Module of {
        name: string;
        signature: t list;
        body: t list;
      }
    | ModuleAlias of { name: string; target: t }
    | Functor of {
        name: string;
        args: functor_arg list;
        body: t list;
      }
    | ModuleType of {
        name: string;
        body: t list;
      }
    | FunctorApply of { callee: t; argument: t }
    | Constraint of {
        expr: t;
        signature: t list;
      }
    | Typeof of t
    | WithConstraint of {
        base: t;
        constraints: t list;
      }
    | BindModules of {
        modules: bound_module list;
        scope: t list;
      }
    | Scope of t list

  val serializer: t Serde.Ser.t

  val deserializer: t Serde.De.t
end

type source_kind =
  | Implementation
  | Interface
type source_summary = {
  source: Path.t;
  source_hash: Crypto.hash;
  module_path: string list option;
  kind: source_kind;
  items: Item.t list;
}

val source_kind_serializer: source_kind Serde.Ser.t

val source_kind_deserializer: source_kind Serde.De.t

val source_summary_serializer: source_summary Serde.Ser.t

val source_summary_deserializer: source_summary Serde.De.t

type provider = {
  path: string list;
  free_names: string list;
  exports: string list list;
}

module Env: sig
  type t

  val empty: t

  val make: provider list -> t

  val add_provider: t -> provider -> t

  val add_providers: t -> provider list -> t

  val add_path: t -> path:string list -> free_names:string list -> t

  val add_external_summaries: t -> source_summary list -> t

  val with_summaries: t -> source_summary list -> t

  val providers: t -> provider list
end

module Resolution: sig
  type t = private {
    modules: string list;
    unresolved: string list;
  }

  val make: modules:string list -> unresolved:string list -> t

  val modules: t -> string list

  val unresolved: t -> string list
end

module ResolvedSource: sig
  type t = private {
    source: Path.t;
    source_hash: Crypto.hash;
    module_path: string list option;
    modules: string list;
    unresolved: string list;
  }

  val source: t -> Path.t

  val source_hash: t -> Crypto.hash

  val module_path: t -> string list option

  val modules: t -> string list

  val unresolved: t -> string list
end

type parse_error =
  | Parse_diagnostics of Syn.Diagnostic.t list
type resolve_error =
  | Invalid_provider of string

val analyze:
  ?implicit_opens:string list list ->
  ?module_path:string list ->
  source:Path.t ->
  source_hash:Crypto.hash ->
  Syn.Parser.parse_result ->
  (source_summary, parse_error) result

val collect:
  ?implicit_opens:string list list ->
  ?module_path:string list ->
  source:Path.t ->
  source_hash:Crypto.hash ->
  Syn.Parser.parse_result ->
  (source_summary, parse_error) result

val resolve: Env.t -> source_summary list -> (ResolvedSource.t list, resolve_error) result

module Ir: sig
  module Item = Item

  type nonrec source_kind = source_kind =
    | Implementation
    | Interface
  type nonrec source_summary = source_summary = {
    source: Path.t;
    source_hash: Crypto.hash;
    module_path: string list option;
    kind: source_kind;
    items: Item.t list;
  }
  type nonrec parse_error = parse_error =
    | Parse_diagnostics of Syn.Diagnostic.t list

  val source_kind_serializer: source_kind Serde.Ser.t

  val source_kind_deserializer: source_kind Serde.De.t

  val source_summary_serializer: source_summary Serde.Ser.t

  val source_summary_deserializer: source_summary Serde.De.t

  val analyze:
    ?implicit_opens:string list list ->
    ?module_path:string list ->
    source:Path.t ->
    source_hash:Crypto.hash ->
    Syn.Parser.parse_result ->
    (source_summary, parse_error) result

  val collect:
    ?implicit_opens:string list list ->
    ?module_path:string list ->
    source:Path.t ->
    source_hash:Crypto.hash ->
    Syn.Parser.parse_result ->
    (source_summary, parse_error) result
end
