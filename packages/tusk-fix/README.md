# tusk_fix - OCaml Linter and Fixer

A pipeline-based linter and code fixer for OCaml, built on top of the `syn` parser.

## Architecture

### Core Design Principles

1. **Parse Once**: Source code is parsed exactly once into a `syn` AST
2. **Pipeline Processing**: The AST flows through a pipeline of linters and fixers
3. **Structured Diagnostics**: All issues are reported as structured `Diagnostic` objects
4. **Multiple Output Formats**: Diagnostics can be rendered as human-readable text or machine-readable JSON
5. **Extensible Rules**: Easy to add new linting rules

### Components

```
Source Code
     ↓
  Syn.parse → AST + Parse Diagnostics
     ↓
  Pipeline (Rule₁, Rule₂, ..., Ruleₙ)
     ↓
  Diagnostic Collection
     ↓
  Reporter (Text | JSON)
```

#### 1. Diagnostic

Represents a single issue found in the code:

```ocaml
type severity = Error | Warning | Info | Hint

type t = {
  severity : severity;
  message : string;
  span : Syn.Ceibo.Span.t;  (* Source location *)
  rule_id : string;          (* Which rule detected this *)
  suggestion : string option; (* Optional fix suggestion *)
}
```

Diagnostics can be:
- Converted to human-readable strings (`Diagnostic.to_string`)
- Serialized to JSON for tooling (`Diagnostic.to_json`)

#### 2. Rule

A linting rule that inspects the AST and produces diagnostics:

```ocaml
type t = {
  id : string;
  name : string;
  description : string;
  enabled : bool;
  run : Syn.Ceibo.Green.t -> Diagnostic.t list;
}
```

Rules:
- Receive the parsed AST (green tree)
- Traverse the tree looking for patterns
- Emit `Diagnostic` values for issues found
- Can be enabled/disabled

#### 3. Pipeline

Orchestrates the linting process:

```ocaml
type result = {
  tree : Syn.Ceibo.Green.t;
  diagnostics : Diagnostic.t list;
}

val run : t -> string -> result
```

The pipeline:
1. Parses source code once using `Syn.parse`
2. Collects parse errors as diagnostics
3. Runs each enabled rule on the AST
4. Aggregates all diagnostics
5. Returns the tree + diagnostics

#### 4. Reporter

Renders diagnostics in different formats:

```ocaml
type format = Text | Json

val report : format:format -> Diagnostic.t list -> unit
```

Formats:
- **Text**: Human-readable error messages for CLI output
- **JSON**: Machine-readable format for IDE/tooling integration

## Usage

### Command Line

```bash
# Lint a file (text output)
tusk_fix src/main.ml

# JSON output for tooling
tusk_fix --format=json src/main.ml

# Exit codes:
#   0 - No issues found
#   1 - Issues found or error occurred
```

### Programmatic

```ocaml
open Std

(* Create a custom pipeline *)
let pipeline = Pipeline.make 
  ~rules:[
    Snake_case_type_names.make ();
    (* Add more rules here *)
  ] 
  ()

(* Run on source code *)
let source = Fs.read (Path.v "file.ml") |> Result.unwrap in
let result = Pipeline.run pipeline source in

(* Process results *)
match result.diagnostics with
| [] -> Log.info "All good!"
| diagnostics -> 
    Reporter.report ~format:Text diagnostics
```

## Built-in Rules

### snake-case-type-names

**Rule ID**: `snake-case-type-names`

Detects type declarations that use camelCase instead of `snake_case`.

**Example**:

```ocaml
(* BAD *)
type userProfile = {
  name : string;
}

(* GOOD *)
type user_profile = {
  name : string;
}
```

## Creating Custom Rules

### Step 1: Implement the Rule

Create a new file in `src/rules/`:

```ocaml
(* src/rules/my_rule.ml *)
open Std

let rule_id = "my-rule"
let rule_name = "My Custom Rule"
let rule_description = "Detects some pattern in the code"

let check_tree tree =
  let diagnostics = ref [] in
  
  let rec traverse node =
    match node with
    | Syn.Ceibo.Green.Node n ->
        (* Inspect node, check for patterns *)
        Array.iter traverse (Syn.Ceibo.Green.children_of_node n)
    | Syn.Ceibo.Green.Token t ->
        (* Inspect token *)
        ()
  in
  
  traverse tree;
  !diagnostics

let make () =
  Rule.make ~id:rule_id ~name:rule_name 
    ~description:rule_description
    ~run:check_tree ()
```

### Step 2: Add to Pipeline

Update `pipeline.ml`:

```ocaml
let default_rules () = [
  Snake_case_type_names.make ();
  My_rule.make ();  (* Add your rule *)
]
```

### Rule Writing Guide

**Traversal Pattern**:

```ocaml
let check_tree tree =
  let diagnostics = ref [] in
  
  let rec traverse node =
    match node with
    | Syn.Ceibo.Green.Node n ->
        (* Check node kind *)
        let kind = Syn.Ceibo.Green.kind_of_node n in
        
        (* Get children *)
        let children = Syn.Ceibo.Green.children_of_node n in
        
        (* Recurse *)
        Array.iter traverse children
        
    | Syn.Ceibo.Green.Token t ->
        (* Get token text and kind *)
        let text = Syn.Ceibo.Green.text_of_token t in
        let kind = Syn.Ceibo.Green.kind_of_token t in
        
        (* Check for issues *)
        if is_problematic text then
          let span = compute_span () in
          let diag = Diagnostic.make
            ~severity:Warning
            ~message:"Problem detected"
            ~span
            ~rule_id
            ~suggestion:"Try this instead"
            ()
          in
          diagnostics := diag :: !diagnostics
  in
  
  traverse tree;
  !diagnostics
```

**Emitting Diagnostics**:

```ocaml
(* Warning with suggestion *)
Diagnostic.make 
  ~severity:Warning
  ~message:"Type names should use snake_case instead of camelCase."
  ~span
  ~rule_id:"snake-case-type-names"
  ~suggestion:"Rename userProfile to user_profile"
  ()

(* Error without suggestion *)
Diagnostic.make 
  ~severity:Error
  ~message:"Invalid syntax"
  ~span
  ~rule_id:"syntax-error"
  ()
```

## Future: User-Defined Lints

The architecture is designed to support user-land lints in the future:

```toml
# .tusk_fix.toml (future)
[rules]
enable = ["snake-case-type-names", "custom-rule"]

[rules.custom-rule]
plugin = "./lints/custom_rule.cmxs"
config = { threshold = 10 }
```

Users will be able to:
1. Write custom rules as OCaml modules
2. Compile them to plugins (`.cmxs`)
3. Load them dynamically via configuration
4. Share rules across projects/teams

## Implementation Status

✅ Core infrastructure
  - ✅ Diagnostic type with severity levels
  - ✅ Rule abstraction
  - ✅ Pipeline architecture
  - ✅ Text reporter
  - ✅ JSON reporter
  - ✅ CLI interface

✅ Built-in rules
  - ✅ snake-case-type-names

🚧 Future work
  - ⬜ More built-in rules
  - ⬜ Auto-fix capabilities
  - ⬜ Configuration file support
  - ⬜ User-defined lint plugins
  - ⬜ IDE integration (LSP)
  - ⬜ Incremental linting
