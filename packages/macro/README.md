# Macro - Compile-Time Code Generation for Tusk

A macro system for Tusk that enables compile-time expansion of domain-specific languages (DSLs) embedded in OCaml code.

## Overview

The macro system allows you to:

1. **Register macro expanders** - Functions that transform macro invocations into OCaml code
2. **Parse embedded DSLs** - Use domain-specific parsers (SQL, regex, JSON, etc.) at compile time
3. **Generate type-safe code** - Transform DSL trees into OCaml expressions
4. **Extend the language** - Add new syntax without modifying the compiler

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     Tusk Compiler                            │
│                                                              │
│  1. Parse OCaml     →    2. Expand Macros                   │
│     (via Syn)            (via Macro)                         │
│         ↓                     ↓                              │
│     OCaml CST            Expanded CST                        │
│         ↓                     ↓                              │
│  3. Type Check       →    4. Codegen                        │
└─────────────────────────────────────────────────────────────┘

                    ┌─────────────────┐
                    │  Macro Package  │
                    ├─────────────────┤
                    │ • Registry      │
                    │ • Expander API  │
                    │ • Tree walking  │
                    │ • Helpers       │
                    └─────────────────┘
                            ↑
                            │ uses
         ┌──────────────────┼──────────────────┐
         ↓                  ↓                   ↓
    ┌─────────┐       ┌─────────┐        ┌─────────┐
    │  Sqlx   │       │  Regex  │        │  Json   │
    │ Package │       │ Package │        │ Package │
    ├─────────┤       ├─────────┤        ├─────────┤
    │• Parser │       │• Parser │        │• Parser │
    │• Expander│      │• Expander│       │• Expander│
    └─────────┘       └─────────┘        └─────────┘
```

## Core Concepts

### 1. Macro Invocation

In source code, macros are invoked using the `name!` syntax:

```ocaml
let query = sql! "SELECT id, name FROM users WHERE age > 18"
let pattern = regex! "^[a-z]+@[a-z]+\.[a-z]+$"
let config = json! "{ \"host\": \"localhost\", \"port\": 5432 }"
```

The Syn parser recognizes these as `MACRO_INVOCATION` nodes in the CST.

### 2. Macro Expander

A macro expander is a function that:
- Takes a `MACRO_INVOCATION` node from the OCaml CST
- Extracts and parses the DSL content
- Generates OCaml code as a Ceibo green tree

```ocaml
type expansion_context = {
  source_file : Path.t;
  span : Ceibo.Span.t;
  (* Additional context: imports, module path, etc. *)
}

type expander = 
  expansion_context -> 
  Ceibo.Red.syntax_node -> 
  Ceibo.Green.node
```

### 3. Registry

The macro registry maps macro names to their expanders:

```ocaml
val register : string -> expander -> expander
val expand_all : Ceibo.Red.syntax_node -> Ceibo.Green.node
```

## Usage Example

### Defining a Macro (DSL Package)

```ocaml
(* packages/sqlx/src/macros.ml *)
open Std
open Macro

(* SQL-specific parser using Ceibo *)
module SqlParser = struct
  type sql_kind = 
    | SQL_SELECT 
    | SQL_INSERT 
    | SQL_WHERE 
    | SQL_COLUMN
    | SQL_TABLE

  (* Uses Ceibo with sql_kind *)
  let parse : string -> (sql_kind, string) Ceibo.Green.node = ...
end

let expand_sql ctx node =
  (* 1. Extract string argument from macro invocation *)
  let sql_string = 
    Macro.extract_string_arg node 
    |> Option.expect ~msg:"sql! requires a string literal" 
  in
  
  (* 2. Parse SQL into its own green tree *)
  let sql_tree = SqlParser.parse sql_string in
  
  (* 3. Validate SQL tree *)
  validate_sql_tree sql_tree;
  
  (* 4. Extract semantic information *)
  let tables = extract_tables sql_tree in
  let columns = extract_columns sql_tree in
  
  (* 5. Generate OCaml code *)
  let generated = format 
    "Sqlx.Query.make ~tables:[%s] ~columns:[%s] ~sql:%S ()"
    (list_to_string tables)
    (list_to_string columns)
    sql_string
  in
  
  (* 6. Parse generated OCaml back into green tree *)
  Macro.quote generated

(* Register the macro *)
let () = Macro.register "sql" expand_sql
```

### Using a Macro (Application Code)

```ocaml
(* app/queries.ml *)
open Sqlx

let get_adult_users db =
  (* At compile time, this expands to: *)
  (* Sqlx.Query.make ~tables:["users"] ~columns:["id"; "name"] ~sql:"..." () *)
  let query = sql! "SELECT id, name FROM users WHERE age > 18" in
  Sqlx.execute db query
```

### Compiler Integration

```ocaml
(* packages/tusk/src/compile.ml *)
open Std

let compile_file path =
  (* 1. Parse OCaml source into CST *)
  let source = Fs.read path |> Result.unwrap in
  let ocaml_green = Syn.Parser.parse source |> Result.unwrap in
  let ocaml_red = Ceibo.Red.new_root ocaml_green in
  
  (* 2. Expand all registered macros *)
  let expanded_green = Macro.expand_all ocaml_red in
  
  (* 3. Continue with type checking *)
  let typed_ast = Typechecker.check expanded_green in
  
  (* 4. Generate code *)
  Codegen.emit typed_ast
```

## Key Design Principles

### 1. Each DSL is Independent

Every DSL (SQL, regex, JSON) uses its own instance of Ceibo with DSL-specific kinds:

```ocaml
(* OCaml CST *)
type ocaml_kind = LET_BINDING | FUNCTION_CALL | MACRO_INVOCATION | ...

(* SQL CST *)
type sql_kind = SQL_SELECT | SQL_WHERE | SQL_COLUMN | ...

(* Regex CST *)
type regex_kind = REGEX_CHAR_CLASS | REGEX_GROUP | REGEX_QUANTIFIER | ...
```

### 2. Lossless DSL Parsing

DSL parsers should use Ceibo to create lossless CSTs, preserving:
- Comments in SQL queries
- Whitespace for pretty-printing
- Source positions for error messages

### 3. Type Safety

Macros generate type-safe OCaml code that is type-checked after expansion:

```ocaml
(* Before expansion *)
let query = sql! "SELECT id, name FROM users"

(* After expansion *)
let query = Sqlx.Query.make 
  ~tables:["users"] 
  ~columns:["id"; "name"] 
  ~sql:"SELECT id, name FROM users" 
  ()

(* Type: Sqlx.Query.t *)
```

### 4. Error Reporting

Macro errors should report positions in both:
- The OCaml source file
- The embedded DSL string

```
Error: Invalid SQL syntax
  --> src/queries.ml:10:15
   |
10 |   let query = sql! "SELECT * FORM users"
   |               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
   |                             ^^^^ Expected 'FROM', found 'FORM'
```

## API Surface

### Core Functions

```ocaml
(* Registration *)
val register : string -> expander -> expander

(* Expansion *)
val expand_all : Ceibo.Red.syntax_node -> Ceibo.Green.node

(* Helpers for macro implementers *)
val extract_string_arg : Ceibo.Red.syntax_node -> string option
val extract_args : Ceibo.Red.syntax_node -> Ceibo.Red.syntax_element list
val quote : string -> Ceibo.Green.node  (* Parse OCaml code into green tree *)

(* Context *)
type expansion_context = {
  source_file : Path.t;
  span : Ceibo.Span.t;
}

val get_context : Ceibo.Red.syntax_node -> expansion_context
```

### Advanced Features (Future)

```ocaml
(* Multi-argument macros *)
val register_multi : string -> (expansion_context -> Ceibo.Red.syntax_node list -> Ceibo.Green.node) -> unit

(* Macro hygiene - generate fresh identifiers *)
val gensym : string -> string

(* Quasi-quoting - embed values in generated code *)
val quasi_quote : string -> (string * Ceibo.Green.node) list -> Ceibo.Green.node
```

## Examples of DSL Macros

### SQL Macro

```ocaml
(* Compile-time SQL validation and type generation *)
let users = sql! "SELECT id, name, email FROM users WHERE active = true"
(* Type: Sqlx.Query.t with columns: id:int, name:string, email:string *)
```

### Regex Macro

```ocaml
(* Compile-time regex validation and optimization *)
let email_pattern = regex! "^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$"
(* Type: Regex.t *)
```

### JSON Schema Macro

```ocaml
(* Compile-time JSON schema validation and type generation *)
let config_schema = json_schema! {|
  {
    "type": "object",
    "properties": {
      "host": { "type": "string" },
      "port": { "type": "integer" }
    }
  }
|}
(* Generates: type config = { host: string; port: int } *)
```

### Template Macro

```ocaml
(* Compile-time template parsing *)
let html = html! "<div class='user'>{{name}}</div>"
(* Type: Html.Template.t with hole: name:string *)
```

## Implementation Strategy

### Phase 1: Core Infrastructure
1. Create `Macro` module with registry
2. Implement tree walking to find `MACRO_INVOCATION` nodes
3. Add helper functions (`quote`, `extract_string_arg`)
4. Integrate into Tusk compiler pipeline

### Phase 2: First DSL Example
1. Build SQL parser using Ceibo
2. Implement `sql!` macro expander
3. Add error reporting
4. Write tests

### Phase 3: Macro Ecosystem
1. Document macro API
2. Create example macros (regex, json)
3. Build tooling for macro debugging
4. Add macro hygiene support

## Design Questions

### Where Should Macros Live?

**Option 1: Separate Package (Current)**
- ✅ Clear separation of concerns
- ✅ Optional dependency
- ✅ Can version independently
- ❌ Extra package for core feature

**Option 2: Part of Std**
- ✅ Available everywhere
- ✅ One less dependency
- ❌ Couples macro system to stdlib
- ❌ Not useful outside Tusk

**Recommendation:** Keep as separate package. Macros are compile-time only and not needed at runtime.

### Syntax

Current proposal: `macro_name! "string"`

Alternatives:
- `%macro_name "string"` (OCaml PPX style)
- `@macro_name "string"` (Rust proc macro style)
- `${macro_name "string"}` (Template style)

**Recommendation:** Stick with `!` - it's distinctive and already parsed by Syn.

### Execution Model

**Option 1: Compile-Time Only (Current)**
- Macros expand during compilation
- Generated code is type-checked and compiled
- No runtime overhead

**Option 2: Staged Compilation**
- Macros can inspect type information
- Requires multi-pass compilation
- More complex implementation

**Recommendation:** Start with compile-time only. Add staged compilation later if needed.

## Related Work

- **Rust procedural macros** - Compile-time code generation
- **Lisp macros** - AST transformation with hygiene
- **Template Haskell** - Staged meta-programming
- **OCaml PPX** - AST rewriting after parsing

## References

- [Ceibo Red-Green Trees](/packages/syn/src/ceibo/README.md)
- [Syn Parser Architecture](/packages/syn/docs/swift-inspired-parser-architecture.md)
- [Rust Macros Book](https://doc.rust-lang.org/book/ch19-06-macros.html)
