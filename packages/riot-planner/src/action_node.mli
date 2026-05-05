open Std
open Std.Collections
open Riot_model

module G = Std.Graph.SimpleGraph

type action_spec = {
  actions: Action.t list;
  outs: Path.t list;
  srcs: Path.t list;
  package: Package.t;
  toolchain: Riot_toolchain.t;
  hash: Crypto.hash;
}
(**
   Create an action_spec with a pre-computed Merkle hash.

   The hash is computed immediately and stored in the action_spec. This enables
   O(1) hash lookups and ensures the action graph is fully hashed when
   construction completes.

   @param dependency_hashes Function to look up hash of a dependency by ID
   @param deps List of dependency node IDs for this action
*)
type t = action_spec G.node

val id: t -> G.Node_id.t

val value: t -> action_spec

val deps: t -> G.Node_id.t list

val make:
  actions:Action.t list ->
  outs:Path.t list ->
  srcs:Path.t list ->
  package:Package.t ->
  toolchain:Riot_toolchain.t ->
  dependency_hashes:(G.Node_id.t -> Crypto.hash) ->
  deps:G.Node_id.t list ->
  action_spec

(**
   Get the pre-computed hash of an action node.

   The hash is computed when the node is created via `make` and includes: 1.
   Package name 2. Toolchain hash (compiler binary) 3. All actions in the node
   (sources, flags, includes, etc.) 4. Source file contents (hashed) 5.
   Expected outputs (paths only, since contents don't exist yet) 6. Hashes of
   all dependency nodes (recursive Merkle property)

   This means if ANY source file changes OR any dependency changes OR the
   toolchain changes, the hash changes, invalidating the cache for this node
   and all downstream nodes.
*)
val get_hash: t -> Crypto.hash

(** Convert an action node to JSON, including all fields and hash *)
val to_json: t -> Data.Json.t

(** Compare two action nodes structurally (ignoring node IDs) *)
val equal: t -> t -> bool
