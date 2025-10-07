# Parser Implementation Plan

A pragmatic, incremental approach to building a ceibo-based parser for OCaml.

## Philosophy

**Start simple, grow organically.** We'll implement just enough to:
1. Parse real code from our codebase
2. Support formatters and linters  
3. Add features as needed

**Not trying to parse all of OCaml initially** - that's months of work. Instead, we parse the subset we actually use.

## Phase 1: Core Expressions (Week 1-2)

**Goal:** Parse simple OCaml files like our test fixtures.

### 1.1 Literals
- Integer: `42`, `0x2A`, `0o52`, `0b101010`
- Float: `3.14`, `1e10`, `0x1.5p3`
- String: `"hello"`, `{|raw string|}`
- Char: `'a'`, `'\n'`
- Bool: `true`, `false`
- Unit: `()`

### 1.2 Simple Expressions
- Variable reference: `x`, `List.map`
- Function call: `f x`, `List.map f xs`
- Infix operators: `x + y`, `a :: b`
- Parentheses: `(expr)`

### 1.3 Let Bindings
```ocaml
let x = 42
let f x = x + 1
let rec fact n = if n = 0 then 1 else n * fact (n - 1)
```

### 1.4 Basic Patterns
- Variable: `x`
- Wildcard: `_`
- Literal: `42`, `"hello"`
- Tuple: `(x, y, z)`
- Constructor: `Some x`, `Ok value`
- List: `[]`, `[x]`, `x :: xs`

### Deliverable
Parse this file:
```ocaml
let x = 42

let add a b = a + b

let factorial n =
  if n = 0 then 1
  else n * factorial (n - 1)

let sum_list xs =
  match xs with
  | [] -> 0
  | x :: rest -> x + sum_list rest
```

## Phase 2: Control Flow & Pattern Matching (Week 3)

### 2.1 If-Then-Else
```ocaml
if condition then value1 else value2
```

### 2.2 Match Expressions
```ocaml
match expr with
| pattern1 -> expr1
| pattern2 when guard -> expr2
| _ -> default
```

### 2.3 Sequences
```ocaml
expr1;
expr2;
expr3
```

### Deliverable
Parse files with pattern matching like our test fixtures.

## Phase 3: Types & Declarations (Week 4-5)

### 3.1 Type Annotations
```ocaml
let f (x : int) : int = x + 1
```

### 3.2 Type Definitions
```ocaml
type color = Red | Green | Blue
type point = { x : int; y : int }
type 'a option = None | Some of 'a
```

### 3.3 Records
```ocaml
{ field1 = expr1; field2 = expr2 }
{ record with field1 = new_value }
record.field
```

### Deliverable
Parse type definitions and record syntax from our codebase.

## Phase 4: Modules (Week 6)

### 4.1 Module Paths
```ocaml
List.map
Collections.HashMap.create
```

### 4.2 Open Statements
```ocaml
open Std
open Collections
```

### 4.3 Module Definitions (basic)
```ocaml
module M = struct
  let x = 42
end
```

### Deliverable
Parse simple module files from our codebase.

## Phase 5: Advanced Features (Weeks 7+)

**Add these as needed:**
- Functors
- First-class modules
- Objects and classes
- Polymorphic variants
- GADTs
- Extension nodes (PPX)
- Attributes

## Implementation Strategy

### 1. Define Syntax Kinds

Create `syntax_kind.ml` with OCaml-specific node kinds:

```ocaml
type t =
  (* Trivia *)
  | WHITESPACE
  | COMMENT
  | DOCSTRING
  
  (* Literals *)
  | INT_LITERAL
  | FLOAT_LITERAL
  | STRING_LITERAL
  | CHAR_LITERAL
  | BOOL_LITERAL
  | UNIT_LITERAL
  
  (* Expressions *)
  | IDENT_EXPR
  | APPLY_EXPR
  | INFIX_EXPR
  | IF_EXPR
  | MATCH_EXPR
  | LET_EXPR
  | FUN_EXPR
  | PAREN_EXPR
  | TUPLE_EXPR
  | LIST_EXPR
  | RECORD_EXPR
  | FIELD_ACCESS_EXPR
  | SEQUENCE_EXPR
  
  (* Patterns *)
  | IDENT_PATTERN
  | WILDCARD_PATTERN
  | LITERAL_PATTERN
  | CONSTRUCTOR_PATTERN
  | TUPLE_PATTERN
  | LIST_PATTERN
  | CONS_PATTERN
  | OR_PATTERN
  | AS_PATTERN
  
  (* Declarations *)
  | LET_BINDING
  | TYPE_DECL
  | EXCEPTION_DECL
  | MODULE_DECL
  | OPEN_STMT
  
  (* Types *)
  | TYPE_VAR
  | TYPE_CONSTRUCTOR
  | FUNCTION_TYPE
  | TUPLE_TYPE
  | RECORD_TYPE
  | VARIANT_TYPE
  
  (* Structural *)
  | SOURCE_FILE
  | MATCH_CASE
  | PATTERN_GUARD
  | RECORD_FIELD
  | TYPE_PARAM
  | CONSTRUCTOR_DECL
  
  (* Error Recovery *)
  | ERROR
  | MISSING
```

### 2. Create Parser Module

`parser.ml` with this structure:

```ocaml
open Std

type t = {
  tokens : Token.t array;
  mutable position : int;
}

let create tokens = { tokens; position = 0 }

let peek parser =
  if parser.position < Array.length parser.tokens then
    Some parser.tokens.(parser.position)
  else None

let advance parser =
  if parser.position < Array.length parser.tokens then (
    let tok = parser.tokens.(parser.position) in
    parser.position <- parser.position + 1;
    Some tok
  ) else None

let at parser kind =
  match peek parser with
  | Some tok -> tok.Token.kind = kind
  | None -> false

(* Consume token and create green token *)
let consume parser =
  match advance parser with
  | Some tok ->
      let width = tok.span.end_ - tok.span.start in
      let text = (* extract from source *) in
      Ceibo.Green.Token (Ceibo.Green.make_token 
        ~kind:(token_kind_to_syntax_kind tok.kind)
        ~text 
        ~width)
  | None -> (* error recovery *)

(* Skip trivia *)
let rec skip_trivia parser =
  match peek parser with
  | Some tok when is_trivia tok.kind ->
      ignore (advance parser);
      skip_trivia parser
  | _ -> ()

(* Parse functions for each production *)
let rec parse_expr parser = ...
let parse_literal parser = ...
let parse_pattern parser = ...
let parse_let_binding parser = ...
```

### 3. Build Green Trees

Each parse function returns a green node:

```ocaml
let parse_let_binding parser =
  skip_trivia parser;
  
  (* Consume 'let' keyword *)
  let let_tok = consume parser in
  
  (* Check for 'rec' *)
  let rec_tok = 
    if at parser (Keyword Rec) then Some (consume parser)
    else None
  in
  
  (* Parse pattern *)
  let pattern = parse_pattern parser in
  
  (* Consume '=' *)
  skip_trivia parser;
  expect parser Eq;
  let eq_tok = consume parser in
  
  (* Parse expression *)
  let expr = parse_expr parser in
  
  (* Build green node *)
  let children = match rec_tok with
    | Some r -> [| let_tok; r; pattern; eq_tok; expr |]
    | None -> [| let_tok; pattern; eq_tok; expr |]
  in
  
  Ceibo.Green.make_node 
    ~kind:LET_BINDING 
    ~children
```

### 4. Error Recovery

Use "Missing" and "Error" nodes:

```ocaml
let expect parser kind =
  match peek parser with
  | Some tok when tok.kind = kind ->
      consume parser
  | _ ->
      (* Create missing token *)
      Ceibo.Green.Token (Ceibo.Green.make_token
        ~kind:(Missing kind)
        ~text:""
        ~width:0)
```

### 5. Entry Point

```ocaml
let parse_source_file source =
  (* Lex into tokens *)
  let tokens = Lexer.lex source |> Result.unwrap in
  
  (* Create parser *)
  let parser = create (Array.of_list tokens) in
  
  (* Parse top-level items *)
  let items = ref [] in
  while peek parser <> None do
    let item = parse_structure_item parser in
    items := item :: !items
  done;
  
  (* Build source file node *)
  Ceibo.Green.make_node
    ~kind:SOURCE_FILE
    ~children:(Array.of_list (List.rev !items))
```

## Testing Strategy

### Unit Tests

Test each production in isolation:

```ocaml
let test_parse_literal () =
  let source = "42" in
  let tokens = Lexer.lex source in
  let parser = create tokens in
  let green = parse_literal parser in
  assert (green.kind = INT_LITERAL)

let test_parse_let_binding () =
  let source = "let x = 42" in
  (* ... *)
```

### Integration Tests

Parse real files from our codebase:

```bash
# Parse all .ml files
find packages/ -name "*.ml" | while read f; do
  echo "Parsing $f"
  ./tusk run syn parse "$f"
done
```

### Round-Trip Tests

Ensure we preserve all source:

```ocaml
let test_round_trip source =
  let green = parse source in
  let red = Ceibo.Red.new_root green in
  let reconstructed = print_tree red in
  assert (source = reconstructed)
```

## Pragmatic Shortcuts

**Things we can skip initially:**

1. **Full Unicode support** - Handle ASCII well, add Unicode later
2. **PPX attributes** - Parse as unknown for now
3. **Object system** - Almost never used in our code
4. **Polymorphic variants** - Add when needed
5. **Format strings** - Treat as regular strings
6. **Quoted strings** - Basic support only

**Things we MUST get right:**

1. **Trivia handling** - Comments and whitespace crucial for formatter
2. **Error recovery** - Must handle incomplete files
3. **Span tracking** - Accurate positions for errors
4. **Common expressions** - let, match, if, function calls

## Success Metrics

**Phase 1 Success:**
- ✅ Parse 80% of files in `packages/kernel/src`
- ✅ Reconstruct source perfectly (round-trip)
- ✅ Handle syntax errors gracefully

**Phase 2 Success:**
- ✅ Parse 90% of files in `packages/std/src`
- ✅ Support formatter use cases
- ✅ Support basic linter traversal

**Phase 3 Success:**
- ✅ Parse 95% of files in entire codebase
- ✅ Handle type definitions
- ✅ Support macro system hooks

## Timeline Estimate

- **Week 1-2:** Literals, simple expressions, let bindings
- **Week 3:** Pattern matching, control flow
- **Week 4-5:** Types and declarations
- **Week 6:** Modules
- **Week 7+:** Polish and advanced features

**Total: 6-8 weeks to useful parser, 12+ weeks for complete coverage**

## Next Steps

1. Create `syntax_kind.ml` with node types
2. Create basic `parser.ml` structure
3. Implement literal parsing
4. Add tests
5. Iterate!
