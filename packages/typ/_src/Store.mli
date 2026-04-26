open Std
open Model

(**
   Semantic persistence layer for [typ] built on top of [Contentstore].

   [Typ.Store] hides the storage details needed to persist and reload
   canonical [ModuleTypings] values while keeping [typ] itself generic over
   the concrete host cache implementation.
*)
type t
type package_bundle = {
  fingerprint: Crypto.hash;
  typings: ModuleTypings.t list;
}

(** Build a [Typ.Store] on top of a generic [Contentstore]. *)
val create: Contentstore.t -> unit -> t

(** Load canonical module typings for one module name, when present. *)
val load_module_typings: t -> module_name:string -> ModuleTypings.t option

(** Load canonical module typings by their source hash, when present. *)
val load_module_typings_by_hash: t -> source_hash:Crypto.hash -> ModuleTypings.t option

(** Load all canonical module typings persisted for one package, when present. *)
val load_package_module_typings: t -> package_name:string -> ModuleTypings.t list option

(**
   Load the persisted package bundle, including its transitive typing
   fingerprint, when present.
*)
val load_package_bundle: t -> package_name:string -> package_bundle option

(**
   Persist canonical module typings in both hash-addressed and module-name
   indexed forms.
*)
val save_module_typings: t -> ModuleTypings.t -> (unit, string) result

(**
   Persist the canonical module typings bundle for one package.

   Hosts use this to cache the locally computed module-typing closure for a
   package under the current build lane.
*)
val save_package_module_typings:
  t ->
  package_name:string ->
  ModuleTypings.t list ->
  (unit, string) result

(**
   Persist a package bundle together with the transitive fingerprint that
   proves which source and dependency interface inputs produced it.
*)
val save_package_bundle:
  t ->
  package_name:string ->
  fingerprint:Crypto.hash ->
  ModuleTypings.t list ->
  (unit, string) result
