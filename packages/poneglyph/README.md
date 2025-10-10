# Poneglyph - EAV Graph Store for Build Metadata

**A lightweight, in-memory entity-attribute-value graph database for tracking build system state, file metadata, and semantic code information.**

## Overview

Poneglyph provides a simple graph store that makes it easy to track relationships between files, packages, modules, and build artifacts. It's designed to eliminate "build archaeology" - the expensive grep/find operations that developers and LLMs spend time doing to understand codebase structure.

## Quick Start

```ocaml
open Std
open Poneglyph

(* Create a graph store *)
let graph = create () in

(* Define entities as URIs *)
let file_uri = Uri.of_string "tusk:file:src/path.ml:abc123" in
let formatted_attr = Uri.of_string "tusk:fmt:formatted" in
let timestamp_attr = Uri.of_string "tusk:fmt:timestamp" in

(* State facts about entities *)
state graph Fact.[
  fact file_uri formatted_attr (Value.Bool true);
  fact file_uri timestamp_attr (Value.DateTime (Datetime.now()));
];

(* Query the graph *)
match get graph ~entity:file_uri ~attr:formatted_attr with
| Some (Value.Bool true) -> print "Already formatted!"
| _ -> print "Needs formatting"
```

## Core Concepts

### URIs

URIs are the primary identifiers in Poneglyph. They're automatically interned for fast comparison:

```ocaml
let file_uri = Uri.of_string "tusk:file:packages/std/src/path.ml:abc123"
let module_uri = Uri.of_string "ocaml:module:Std.Path"
let package_uri = Uri.of_string "tusk:package:std"
```

**URI Format Convention:**
- `namespace:type:identifier[:version]`
- Examples:
  - `tusk:file:src/main.ml:abc123` - file with content hash
  - `tusk:package:kernel` - package entity
  - `tusk:artifact:def456` - build artifact
  - `ocaml:module:Std.Path` - OCaml module
  - `ocaml:value:Std.Path.v` - OCaml value

### Values

Poneglyph supports rich value types:

```ocaml
type Value.t =
  | String of string        (* "hello" *)
  | Int of int             (* 42 *)
  | Bool of bool           (* true *)
  | Float of float         (* 3.14 *)
  | Uri of Uri.t           (* reference to another entity *)
  | DateTime of Datetime.t (* timestamps *)
  | List of t list         (* collections *)
```

### Facts

Facts are triples of `(entity, attribute, value)`:

```ocaml
state graph Fact.[
  (* File metadata *)
  fact file_uri (Uri.of_string "tusk:content_hash") (Value.String "abc123");
  fact file_uri (Uri.of_string "tusk:size_bytes") (Value.Int 4096);
  
  (* Relationships *)
  fact module_uri (Uri.of_string "ocaml:belongs_to") (Value.Uri package_uri);
  fact module_uri (Uri.of_string "ocaml:depends_on") (Value.Uri kernel_module);
  
  (* Lists *)
  fact package_uri (Uri.of_string "tusk:dependencies") 
    (Value.List [Value.Uri dep1; Value.Uri dep2]);
];
```

## Use Cases

### 1. Format Cache

Track which files have been formatted to skip redundant work:

```ocaml
let check_formatted graph path content =
  let hash = Crypto.Hash.Sha256.digest content |> Crypto.Digest.hex in
  let file_uri = Uri.of_string 
    (format "tusk:file:%s:%s" (Path.to_string path) hash) in
  
  match get graph ~entity:file_uri ~attr:(Uri.of_string "tusk:fmt:formatted") with
  | Some (Value.Bool true) -> `AlreadyFormatted
  | _ -> `NeedsFormatting
```

### 2. Build Cache

Track build artifacts by content hash:

```ocaml
let record_build graph package artifact_hash =
  let pkg_uri = Uri.of_string (format "tusk:package:%s" package.name) in
  let artifact_uri = Uri.of_string (format "tusk:artifact:%s" artifact_hash) in
  
  state graph Fact.[
    fact pkg_uri (Uri.of_string "tusk:last_hash") (Value.String artifact_hash);
    fact pkg_uri (Uri.of_string "tusk:built_as") (Value.Uri artifact_uri);
    fact pkg_uri (Uri.of_string "tusk:build_time") (Value.DateTime (Datetime.now()));
    fact artifact_uri (Uri.of_string "tusk:exists") (Value.Bool true);
  ]
```

### 3. Semantic Code Metadata

Store OCaml module dependencies and definitions:

```ocaml
let record_module_info graph module_name file_path =
  let mod_uri = Uri.of_string (format "ocaml:module:%s" module_name) in
  let file_uri = Uri.of_string (format "tusk:file:%s" (Path.to_string file_path)) in
  
  state graph Fact.[
    fact mod_uri (Uri.of_string "ocaml:defined_in") (Value.Uri file_uri);
    fact mod_uri (Uri.of_string "ocaml:provides") 
      (Value.List [
        Value.String "Path.t";
        Value.String "Path.v";
        Value.String "Path.of_string";
      ]);
  ]
```

### 4. File Change Tracking

Track file modifications for incremental builds:

```ocaml
let update_file_metadata graph path =
  let file_uri = Uri.of_string (format "tusk:file:%s" (Path.to_string path)) in
  let content = Fs.read path |> Result.unwrap in
  let hash = Crypto.Hash.Sha256.digest content |> Crypto.Digest.hex in
  
  state graph Fact.[
    fact file_uri (Uri.of_string "tusk:content_hash") (Value.String hash);
    fact file_uri (Uri.of_string "tusk:last_modified") 
      (Value.DateTime (Datetime.now()));
  ]
```

## API Reference

### Creation

```ocaml
val create : unit -> t
```

Create a new empty graph store.

### URIs

```ocaml
module Uri : sig
  val of_string : string -> t
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
end
```

URIs are automatically interned - same string always returns same URI for fast comparison.

### Facts

```ocaml
module Fact : sig
  val fact : Uri.t -> Uri.t -> Value.t -> t
end

val state : t -> Fact.t list -> unit
```

Assert facts into the graph. Updates existing values for the same entity+attribute pair.

### Queries

```ocaml
val get : t -> entity:Uri.t -> attr:Uri.t -> Value.t option
val exists : t -> Uri.t -> bool
```

Simple queries - check if entity exists or get attribute value.

## Design Principles

1. **In-Memory First** - Fast lookups, simple implementation
2. **Content-Addressed** - Use hashes for immutable content
3. **URI-Based** - Human-readable identifiers with automatic interning
4. **Declarative** - State facts, don't imperatively mutate
5. **Extensible** - Add new namespaces and attributes freely

## Future Extensions

- **Persistence** - Save/load graph to disk
- **Datalog Queries** - Transitive relationships, pattern matching
- **Reverse Lookups** - Find all entities with attribute=value
- **Time Travel** - Track fact history with transaction IDs
- **LLM Annotations** - Store semantic knowledge about code patterns

## Related Work

Inspired by:
- **Datomic** - EAV model, time-travel, Datalog queries
- **RDF Triples** - Subject-predicate-object semantic web model
- **Build system caches** - Bazel, Buck2 content-addressed storage

## License

See top-level LICENSE file.
