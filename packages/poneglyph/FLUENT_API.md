# Poneglyph Fluent API

This document describes the fluent API for building facts in Poneglyph.

## Core Insight: Everything is an Entity

In Poneglyph, **everything is just an entity with facts about it**. This includes:
- Your domain data (files, packages, modules, etc.)
- The schema itself (kinds, fields, value types)
- Even relationships are entities!

When you define a schema, you're just creating **facts about entities** that describe your schema.

## Uri Construction

URIs can be built from parts using a list syntax:

```ocaml
let file_uri = Uri.make Uri.[
  ns "tusk";
  kind "file";
  id "src/main.ml"
]
```

This creates the URI `"tusk:file:src/main.ml"`.

### Uri.id supports format strings

```ocaml
let filename = "main" in
let file_uri = Uri.make Uri.[
  ns "tusk";
  kind "file";
  id "src/%s.ml" filename  (* Format string! *)
]
```

## Defining Schemas

Schemas are defined declaratively. Each definition creates an entity (URI) and facts about that entity:

```ocaml
open Poneglyph
open Schema

module Tusk = struct
  let ns = namespace "tusk"

  (* Define a kind - creates the entity tusk:file with schema facts *)
  let file = 
    kind ~ns "file"
    |> doc "A File in the Tusk schema"

  (* Define a field - creates the entity tusk:content_hash with schema facts *)
  let content_hash =
    field ~ns "content_hash"
    |> used_on file
    |> value Type.string
    |> doc "The content hash of a file"

  (* Collect all definitions *)
  let all_defs = [file; content_hash]

  (* Register schema into the store *)
  let register store = Schema.register store all_defs
end
```

### What `kind` and `field` actually do

When you write:
```ocaml
let file = kind ~ns "file" |> doc "A File in the Tusk schema"
```

This creates:
1. The URI `tusk:file`
2. Facts about it:
   - `tusk:file` has `schema:type` = "kind"
   - `tusk:file` has `schema:doc` = "A File in the Tusk schema"

When you write:
```ocaml
let content_hash =
  field ~ns "content_hash"
  |> used_on file
  |> value Type.string
```

This creates:
1. The URI `tusk:content_hash`
2. Facts about it:
   - `tusk:content_hash` has `schema:type` = "field"
   - `tusk:content_hash` has `schema:used_on` = `tusk:file`
   - `tusk:content_hash` has `schema:value_type` = `schema:type/string`

### Schema Types

A definition is just:
```ocaml
type def = Uri.t * Fact.t list
```

It's an entity URI plus the facts that describe it!

### Schema Builders

All schema builders work the same way - they take a `def` and return a `def`:

- **`doc "..."`** - Adds a documentation fact (works for kinds AND fields!)
- **`used_on kind_def`** - Adds a "used_on" fact to a field
- **`value Type.string`** - Adds a value type fact to a field

### Value Types

```ocaml
Schema.Type.string    (* schema:type/string *)
Schema.Type.int       (* schema:type/int *)
Schema.Type.bool      (* schema:type/bool *)
Schema.Type.float     (* schema:type/float *)
Schema.Type.uri       (* schema:type/uri *)
Schema.Type.datetime  (* schema:type/datetime *)
Schema.Type.list Type.uri  (* schema:type/list:schema:type/uri *)
```

## Registering Schemas

Register your schema to store the schema facts:

```ocaml
let store = Poneglyph.create () in
Tusk_schema.register store;
Ocaml_schema.register store;
```

Now you can **query the schema itself**:

```ocaml
let file_kind_uri = fst Tusk_schema.file in
match Poneglyph.get store ~entity:file_kind_uri ~attr:Schema.doc_attr with
| Some (Value.String doc) -> Log.info "Documentation: %s" doc
| _ -> ()
```

## Building Facts

Build facts for entities using `Fact.for_entity`:

```ocaml
let facts = Fact.for_entity file_uri [
  Tusk.content_hash ~hash:"abc123";
  Tusk.size_bytes ~bytes:4096;
  OCaml.belongs_to_package ~package:package_uri;
]
```

This returns `Fact.t list` directly!

### How it works

Internally, `Fact.for_entity` is just a specialized `List.map`:

```ocaml
val for_entity : Uri.t -> (Uri.t -> Fact.t) list -> Fact.t list

let for_entity entity fact_builders = 
  List.map (fun f -> f entity) fact_builders
```

## Complete Example

```ocaml
open Std
open Poneglyph

(* Define your schema *)
module MySchema = struct
  open Schema

  let ns = namespace "myapp"

  let user = kind ~ns "user" |> doc "A user entity"

  let email =
    field ~ns "email"
    |> used_on user
    |> value Type.string
    |> doc "User's email address"

  let age =
    field ~ns "age"
    |> used_on user
    |> value Type.int
    |> doc "User's age"

  let all_defs = [user; email; age]
  let register store = Schema.register store all_defs

  (* Fact builders *)
  let email ~email = Schema.string_value ~field:email ~value:email
  let age ~age = Schema.int_value ~field:age ~value:age
end

(* Use your schema *)
let () =
  let store = Poneglyph.create () in

  (* Register schema - stores schema facts *)
  MySchema.register store;

  (* Create entities and facts *)
  let user_uri = Uri.make Uri.[ns "myapp"; kind "user"; id "alice"] in

  let facts = Fact.for_entity user_uri [
    MySchema.email ~email:"alice@example.com";
    MySchema.age ~age:30;
  ] in

  Poneglyph.state store facts;

  (* Query data *)
  let email_field_uri = fst MySchema.email in
  match Poneglyph.get store ~entity:user_uri ~attr:email_field_uri with
  | Some (Value.String email) -> Log.info "Email: %s" email
  | _ -> ()
```

## Accessing Field URIs

Since definitions are `(Uri.t * Fact.t list)`, extract the URI with `fst`:

```ocaml
let content_hash_uri = fst Tusk.content_hash
match Poneglyph.get store ~entity:file_uri ~attr:content_hash_uri with
| Some (Value.String hash) -> Log.info "Hash: %s" hash
| _ -> ()
```

## Benefits

- **Unified Model**: Everything is an entity - data, schema, relationships
- **Self-Describing**: The schema is stored in the graph itself
- **Queryable Schema**: You can query schema metadata just like data
- **Introspectable**: Tools can discover the schema at runtime
- **Extensible**: Add new schema attributes without changing code
- **Simple**: One concept (EAV) for everything

## Schema Namespace

Schema metadata is stored under the `schema` namespace:

- `schema:type` - Whether something is a "kind" or "field"
- `schema:doc` - Documentation string
- `schema:used_on` - Which kind(s) a field is used on
- `schema:value_type` - The value type of a field

You can query these like any other facts!
