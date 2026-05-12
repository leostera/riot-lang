# Building a Tokenstream/CST-Based Parser for OCaml

A comprehensive guide based on the swift-syntax architecture for building a production-grade OCaml parser suitable for linting, formatting, and code modification.

## Table of Contents

1. [Core Architecture Overview](#core-architecture-overview)
2. [Design Principles](#design-principles)
3. [Memory Architecture](#memory-architecture)
4. [Lexer Design](#lexer-design)
5. [Parser Design](#parser-design)
6. [Syntax Tree Representation](#syntax-tree-representation)
7. [Error Recovery](#error-recovery)
8. [Incremental Parsing](#incremental-parsing)
9. [Code Generation](#code-generation)
10. [Implementation Roadmap](#implementation-roadmap)

---

## Core Architecture Overview

### Three-Layer Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    High-Level API                        │
│  (OcamlSyntax - Safe, typed syntax tree access)         │
└─────────────────────────────────────────────────────────┘
                           ↑
┌─────────────────────────────────────────────────────────┐
│                    Raw Syntax Layer                      │
│  (RawOcamlSyntax - Memory-efficient arena-allocated)    │
└─────────────────────────────────────────────────────────┘
                           ↑
┌─────────────────────────────────────────────────────────┐
│                      Parser Layer                        │
│         (Recursive descent with recovery)                │
└─────────────────────────────────────────────────────────┘
                           ↑
┌─────────────────────────────────────────────────────────┐
│                      Lexer Layer                         │
│    (Token stream with trivia preservation)               │
└─────────────────────────────────────────────────────────┘
```

---

## Design Principles

### 1. Source Fidelity

**Every byte of input must be represented in the output tree.**

```ocaml
(* Input bytes → CST nodes mapping *)
type source_fidelity = {
  (* Including whitespace, comments, invalid UTF-8 *)
  preserves_all_bytes: bool;
  (* Trivia (whitespace/comments) attached to tokens *)
  trivia_preserved: bool;
  (* Even malformed code produces a tree *)
  always_produces_tree: bool;
}
```

**Key Insight from swift-syntax:**
- Never drop tokens on the floor
- Use `SyntaxText` (raw byte buffers) instead of `String` to preserve invalid UTF-8
- Attach trivia (whitespace, comments) to tokens, not stored separately

**OCaml Application:**
```ocaml
(* Don't use OCaml strings for token text *)
type syntax_text = {
  base_address: char ptr;  (* Raw byte pointer *)
  count: int;
}

(* Token with trivia *)
type raw_token = {
  kind: token_kind;
  whole_text: syntax_text;
  text_range: int * int;  (* Excludes trivia *)
  leading_trivia: trivia_piece list;
  trailing_trivia: trivia_piece list;
  presence: source_presence;  (* present | missing *)
}
```

### 2. Resilience

**Parser must produce structure even from malformed input.**

Three error types (swift-syntax approach):
1. **Missing Tokens** - Expected token not found → synthesize with `presence = missing`
2. **Unexpected Tokens** - Skip tokens with lower precedence, wrap in `UnexpectedNodes`
3. **Missing Syntax** - Entire production fails → create `MissingExprSyntax`, etc.

```ocaml
type source_presence = Present | Missing

type raw_expr_syntax =
  | Let_binding of {
      let_keyword: raw_token;
      pattern: raw_pattern_syntax;
      equals: raw_token;  (* Could be missing *)
      body: raw_expr_syntax;
    }
  | Missing_expr  (* Represents failed production *)
```

**Token Precedence for Recovery:**
```ocaml
type token_precedence =
  | Structural_keyword    (* let, match, if - never skip *)
  | Decl_keyword          (* type, module, class *)
  | Expr_keyword          (* fun, function *)
  | Operator
  | Identifier
  | Literal

(* During recovery, can skip tokens with lower precedence *)
```

### 3. Minimal Context Parsing

**Keep parser mostly stateless.**

```ocaml
type parser = {
  arena: parsing_raw_syntax_arena;
  lexemes: lexeme_sequence;
  current_token: lexeme;
  nesting_level: int;  (* Track depth for stack overflow *)
  (* NO: symbol tables, type info, scope context *)
}

(* Mode bits passed as parameters, not stored *)
val parse_pattern : parser -> in_binding:bool -> raw_pattern_syntax
```

**Why?** Enables incremental parsing - less state to save/restore.

---

## Memory Architecture

### Arena Allocation (Key Performance Win)

**Problem:** Individual malloc per syntax node = slow
**Solution:** Bump-pointer arena allocator

```ocaml
type raw_syntax_arena = {
  allocator: bump_ptr_allocator;
  child_refs: raw_syntax_arena_ref Set.t;  (* References to other arenas *)
  source_buffer: bytes;  (* NULL-terminated source *)
}

(* Allocate syntax nodes in contiguous memory *)
type bump_ptr_allocator = {
  mutable slabs: slab list;
  mutable current_slab: slab;
  mutable current_offset: int;
}

type slab = {
  memory: Obj.t array;  (* Raw memory block *)
  size: int;
}
```

**Benefits:**
1. Single allocation for many nodes (4096 byte slabs)
2. No individual reference counting for nodes
3. Entire tree freed when arena deallocated
4. Cache-friendly sequential memory access

**Safety Model:**
```ocaml
(* RawSyntax nodes do NOT own their arena *)
type raw_syntax = {
  pointer: arena_allocated_pointer;  (* Points into arena *)
}

(* High-level API DOES own arena *)
type syntax = {
  raw: raw_syntax;
  arena: retained_raw_syntax_arena;  (* Strong reference *)
}
```

### Parsing Arena

```ocaml
type parsing_raw_syntax_arena = raw_syntax_arena & {
  source_buffer: bytes;  (* Interned, NULL-terminated *)
  parse_trivia_function: syntax_text -> trivia_position -> trivia_piece list;
}

(* Usage *)
let parse_file source =
  let arena = ParsingRawSyntaxArena.create ~parse_trivia:trivia_parser in
  let buffer = arena.intern_source_buffer source in
  let parser = Parser.create arena buffer in
  let tree = Parser.parse_source_file parser in
  Syntax.make_syntax tree arena  (* Transfers ownership *)
```

---

## Lexer Design

### Lexeme-Based Tokenization

**Not traditional token stream - uses lexeme sequence with cursor.**

```ocaml
(* A lexeme = token + trivia + metadata *)
type lexeme = {
  raw_token_kind: token_kind;
  flags: lexeme_flags;  (* isAtStartOfLine, etc *)
  diagnostic: token_diagnostic option;
  start: char ptr;  (* Points into source buffer *)
  leading_trivia_byte_length: int;
  text_byte_length: int;
  trailing_trivia_byte_length: int;
  cursor: lexer_cursor;  (* Can re-lex from here *)
}

(* Cursor holds lexer state *)
type lexer_cursor = {
  input: bytes;
  position: char ptr;
  previous_char: char;
  (* State stack for string interpolation, etc *)
  state_stack: lexer_state list;
}

(* Sequence provides iteration *)
type lexeme_sequence = {
  source_buffer_start: lexer_cursor;
  mutable cursor: lexer_cursor;
  mutable next_token: lexeme;
  lookahead_tracker: lookahead_tracker ptr;
}
```

**Key Operations:**
```ocaml
(* Advance to next token *)
val advance : lexeme_sequence -> lexeme

(* Peek at next without consuming *)
val peek : lexeme_sequence -> lexeme

(* Re-lex from saved cursor (for incremental parsing) *)
val reset_for_split : lexeme_sequence -> split_token:lexeme -> 
                      consumed_prefix:int -> lexeme
```

### Trivia Handling

**Trivia = whitespace + comments + other non-semantic elements**

```ocaml
type trivia_piece =
  | Spaces of int
  | Tabs of int
  | Newline
  | Carriage_return_newline
  | Line_comment of syntax_text
  | Block_comment of syntax_text
  | Doc_comment of syntax_text

(* Lazy parsing - only parse when accessed *)
type trivia_parsing_strategy =
  | Eager  (* Parse during lexing *)
  | Lazy   (* Parse on demand via arena.parse_trivia *)
```

**OCaml-specific trivia:**
```ocaml
type ocaml_trivia_piece =
  | Spaces of int
  | Newline
  | Line_comment of syntax_text  (* (** ... *) or (* ... *) *)
  | Block_comment of syntax_text
  | Doc_comment_above of syntax_text  (* (** ... *) before item *)
  | Doc_comment_inline of syntax_text  (* (**< ... *) after item *)
  | PPX_directive of syntax_text  (* [@@...] comments *)
```

---

## Parser Design

### Recursive Descent with Token Consumption Protocol

**Core pattern from swift-syntax:**

```ocaml
(* 1. Check if at expected token *)
val at : parser -> token_spec -> bool

(* 2. Conditionally consume *)
val consume_if : parser -> token_spec -> raw_token option

(* 3. Unconditionally consume (asserts current token matches) *)
val eat : parser -> token_spec -> raw_token

(* 4. Expect with recovery *)
val expect : parser -> token_spec -> 
  (unexpected:raw_unexpected_nodes option * token:raw_token)
```

### Parsing Functions Structure

**Every production = function returning syntax node**

```ocaml
(* Pattern: let-binding *)
(*   let-binding → 'let' pattern '=' expr *)

let parse_let_binding (parser : parser) : raw_let_binding_syntax =
  (* 1. Unconditionally consume 'let' - caller checked we're at 'let' *)
  let let_keyword = eat parser (Keyword Let) in
  
  (* 2. Recursively parse pattern *)
  let pattern = parse_pattern parser in
  
  (* 3. Expect '=' with recovery *)
  let (unexpected_before_eq, equals) = expect parser (Operator "=") in
  
  (* 4. Recursively parse expression *)
  let body = parse_expr parser in
  
  (* 5. Construct node *)
  RawLetBindingSyntax.make
    ~let_keyword
    ~unexpected_before_eq  (* May be None *)
    ~pattern
    ~equals
    ~body
    ~arena:parser.arena
```

### Sequence Parsing Pattern

**For lists (parameters, tuple elements, etc):**

```ocaml
let parse_tuple_elements parser =
  let elements = ref [] in
  let keep_going = ref None in
  
  (* Parsing loop *)
  while true do
    let element = parse_expr parser in
    keep_going := consume_if parser (Comma);
    
    elements := RawTupleElement.make
      ~expression:element
      ~trailing_comma:!keep_going
      ~arena:parser.arena :: !elements;
    
    if !keep_going = None then ()
  done;
  
  RawTupleElementList.make (List.rev !elements) parser.arena
```

### Lookahead

**Single token lookahead via `peek()` - for more, use `Lookahead`**

```ocaml
(* Check next token without consuming *)
if peek parser (Keyword Then) then
  ...

(* Multi-token lookahead *)
let la = lookahead parser in
la.consume_any_token ();  (* Doesn't affect main parser *)
la.consume_any_token ();
if la.at (Keyword Else) then
  (* Decide what to do in main parser *)
  ...
```

**Important:** Lookahead operates on a COPY of lexeme sequence.

---

## Syntax Tree Representation

### Raw Syntax (Arena-Allocated)

**Two types of nodes:**

1. **Tokens** - Leaf nodes
2. **Layout Nodes** - Fixed children (e.g., let-binding has: keyword, pattern, =, expr)
3. **Collection Nodes** - Variable children of same type (e.g., list of declarations)

```ocaml
(* Type-erased node *)
type raw_syntax = {
  pointer: arena_allocated_pointer;  (* → RawSyntaxData *)
}

type raw_syntax_data = {
  payload: raw_syntax_payload;
  arena_reference: raw_syntax_arena_ref;
}

type raw_syntax_payload =
  | Parsed_token of parsed_token
  | Materialized_token of materialized_token
  | Layout of layout_node
  
type layout_node = {
  kind: syntax_kind;
  layout: raw_syntax option array;  (* Fixed size *)
  byte_length: int;
  descendant_count: int;
  recursive_flags: recursive_flags;
}
```

### Typed Wrappers

**Each syntax node has a typed wrapper:**

```ocaml
(* Protocol *)
module type RawSyntaxNodeProtocol = sig
  val is_kind_of : raw_syntax -> bool
  val raw : t -> raw_syntax
end

(* Concrete node *)
type raw_let_binding_syntax = {
  raw: raw_syntax;
}

let make ~let_keyword ~pattern ~equals ~body ~arena =
  let layout = [|
    Some (RawTokenSyntax.raw let_keyword);
    Some (RawPatternSyntax.raw pattern);
    Some (RawTokenSyntax.raw equals);
    Some (RawExprSyntax.raw body);
  |] in
  let data = Layout {
    kind = LetBinding;
    layout;
    byte_length = compute_length layout;
    descendant_count = count_descendants layout;
    recursive_flags = compute_flags layout;
  } in
  { raw = RawSyntax.make arena data }
```

### High-Level Syntax API

**Safe, user-facing API with strong references:**

```ocaml
type 'a syntax = {
  raw: 'a;  (* Raw syntax node *)
  arena: retained_raw_syntax_arena;  (* Keeps arena alive *)
}

type let_binding_syntax = raw_let_binding_syntax syntax

(* Accessors *)
let let_keyword (node : let_binding_syntax) : token_syntax =
  let raw_token = RawLetBindingSyntax.let_keyword node.raw in
  { raw = raw_token; arena = node.arena }

(* Modification - creates new tree *)
let with_let_keyword (node : let_binding_syntax) (new_kw : token_syntax) 
  : let_binding_syntax =
  let new_raw = RawLetBindingSyntax.with_let_keyword node.raw new_kw.raw in
  { raw = new_raw; arena = node.arena }
```

---

## Error Recovery

### Token Precedence-Based Recovery

**When expecting token T but found X:**

```ocaml
type recovery_strategy =
  | Skip_unexpected  (* X has lower precedence than T *)
  | Synthesize_missing  (* Can't find T *)
  
let expect parser spec =
  (* Try direct match *)
  match consume_if parser spec with
  | Some token -> (None, token)
  | None ->
      (* Try recovery via lookahead *)
      let la = lookahead parser in
      match can_recover_to la spec with
      | Some handle ->
          (* Found expected token after skipping *)
          let (unexpected, token) = eat parser handle in
          (Some unexpected, token)
      | None ->
          (* Give up - synthesize missing *)
          (None, missing_token parser spec)
```

**Example:**
```ocaml
(* Input: let x ys = 42 *)
(*               ^^^ unexpected between pattern and = *)

let parse_let_binding parser =
  let kw = eat parser (Keyword Let) in
  let pat = parse_pattern parser in  (* Parses 'x' *)
  
  (* Expect '=' but found 'ys' *)
  let (unexpected, eq) = expect parser (Operator "=") in
  (* Result: unexpected = Some [ys], eq = '=' token *)
  
  let body = parse_expr parser in
  make ~kw ~unexpected_before_eq:unexpected ~pat ~eq ~body
```

### Missing Syntax Nodes

**When entire production fails:**

```ocaml
type raw_expr_syntax =
  | ...
  | Missing_expr

(* In declaration parser *)
let parse_declaration parser =
  let attrs = parse_attributes parser in
  let modifiers = parse_modifiers parser in
  
  match at_declaration_keyword parser with
  | Some kw -> parse_specific_decl parser kw
  | None ->
      (* Have attributes/modifiers but no declaration *)
      RawMissingDeclSyntax.make
        ~attributes:attrs
        ~modifiers
        ~arena:parser.arena
```

---

## Incremental Parsing

### Node Reuse Strategy

**Key insight: Reuse nodes whose source region is unchanged**

```ocaml
type incremental_parse_transition = {
  old_tree: syntax;
  old_source: bytes;
  new_source: bytes;
  edits: source_edit list;
}

type source_edit = {
  offset: int;
  length: int;  (* Bytes removed *)
  replacement: bytes;
}

(* Lookahead tracking *)
type lookahead_tracker = {
  mutable furthest_offset: int;
}

type lookahead_ranges = {
  (* For each node, how far ahead parser looked *)
  ranges: (raw_syntax_id, int) Hashtbl.t;
}
```

**Reuse decision:**
```ocaml
let can_reuse_node node edit_offset lookahead_ranges =
  let node_start = node_offset node in
  let node_end = node_start + byte_length node in
  let lookahead_end = node_start + (get_lookahead_length lookahead_ranges node) in
  
  (* Node is reusable if no edit touches it or its lookahead range *)
  edit_offset < node_start || edit_offset >= lookahead_end
```

**Usage:**
```ocaml
let incremental_parse old_tree old_source new_source edits =
  let transition = { old_tree; old_source; new_source; edits } in
  let arena = ParsingRawSyntaxArena.create () in
  let buffer = arena.intern_source_buffer new_source in
  
  let parser = Parser.create arena buffer ~transition:(Some transition) in
  Parser.parse_source_file parser  (* Reuses unchanged nodes *)
```

---

## Code Generation

### Define Grammar Declaratively

**Instead of handwriting all typed wrappers:**

```ocaml
(* In OcamlSyntaxSupport/ExprNodes.ml *)

let let_binding = Node.make
  ~kind:LetBinding
  ~base:Expr
  ~name_for_diagnostics:"let binding"
  ~documentation:"A let binding expression"
  ~children:[
    Child.make
      ~name:"LetKeyword"
      ~kind:(Token (Keyword Let));
    Child.make
      ~name:"Pattern"
      ~kind:(Node PatternSyntax);
    Child.make
      ~name:"Equals"
      ~kind:(Token (Operator "="));
    Child.make
      ~name:"Body"
      ~kind:(Node ExprSyntax);
  ]
```

**Generate:**
1. `RawLetBindingSyntax` type definition
2. Constructor function
3. Accessor functions
4. `LetBindingSyntax` high-level wrapper
5. Visitor patterns

```bash
# Run generator
dune exec generate-ocaml-syntax

# Outputs:
# - OcamlSyntax/generated/raw/RawSyntaxNodes*.ml
# - OcamlSyntax/generated/SyntaxNodes*.ml
# - OcamlParser/generated/ParserEntryPoints.ml
```

---

## Implementation Roadmap

### Phase 1: Foundation (Weeks 1-3)

**Memory Infrastructure:**
- [ ] Implement `BumpPtrAllocator`
- [ ] Implement `RawSyntaxArena` with child references
- [ ] Implement `SyntaxText` (byte buffer, no UTF-8 validation)
- [ ] Arena-allocated arrays (`ArenaAllocatedBufferPointer`)

**Basic Types:**
- [ ] `TokenKind` enumeration for OCaml tokens
- [ ] `SyntaxKind` enumeration for syntax node types
- [ ] `RawSyntax` and `RawSyntaxData` core types
- [ ] `SourcePresence` (present/missing)

### Phase 2: Lexer (Weeks 4-6)

**Lexer Core:**
- [ ] `LexerCursor` with state stack
- [ ] Basic tokenization (keywords, operators, identifiers)
- [ ] `Lexeme` structure with trivia
- [ ] `LexemeSequence` with advance/peek

**OCaml-Specific Lexing:**
- [ ] String literals (with escape sequences)
- [ ] Comments (line, block, doc comments)
- [ ] Numeric literals (int, float, with separators)
- [ ] Operators (infix, prefix, indexing)

**Trivia:**
- [ ] Lazy trivia parsing
- [ ] Doc comment recognition
- [ ] PPX attribute detection in comments

### Phase 3: Parser Foundation (Weeks 7-10)

**Parser Core:**
- [ ] `Parser` struct with arena and lexeme sequence
- [ ] Token consumption protocol (`at`, `consume_if`, `eat`, `expect`)
- [ ] Lookahead mechanism
- [ ] Nesting level tracking

**Recovery:**
- [ ] Token precedence hierarchy
- [ ] `canRecoverTo` with lookahead
- [ ] Missing token synthesis
- [ ] Unexpected nodes collection

**Basic Productions:**
- [ ] Parse literals
- [ ] Parse identifiers and paths
- [ ] Parse simple expressions (variables, applications)

### Phase 4: Expression Parser (Weeks 11-14)

- [ ] Let bindings (let, let rec, let ... in)
- [ ] Function definitions
- [ ] Function applications
- [ ] Tuples and records
- [ ] Pattern matching (match, function)
- [ ] Conditionals (if-then-else)
- [ ] Operators (infix, prefix) with precedence
- [ ] Type annotations

### Phase 5: Pattern Parser (Weeks 15-16)

- [ ] Variable patterns
- [ ] Constructor patterns
- [ ] Tuple patterns
- [ ] Record patterns
- [ ] Or-patterns
- [ ] As-patterns
- [ ] Wildcard patterns

### Phase 6: Type Parser (Weeks 17-18)

- [ ] Type variables
- [ ] Type constructors
- [ ] Function types
- [ ] Tuple types
- [ ] Record types
- [ ] Variant types
- [ ] Polymorphic variants
- [ ] Object types

### Phase 7: Declaration Parser (Weeks 19-22)

- [ ] Value declarations (let)
- [ ] Type declarations
- [ ] Module declarations
- [ ] Module types
- [ ] Class declarations
- [ ] Signature items
- [ ] Structure items

### Phase 8: Module System (Weeks 23-25)

- [ ] Module expressions
- [ ] Module types
- [ ] Functors
- [ ] Includes
- [ ] Opens

### Phase 9: Code Generation (Weeks 26-28)

**Generator Infrastructure:**
- [ ] Grammar specification DSL
- [ ] Node definitions for all OCaml syntax
- [ ] Template-based code generation
- [ ] Generate typed raw syntax nodes
- [ ] Generate high-level syntax API
- [ ] Generate visitor patterns

### Phase 10: Polish & Optimization (Weeks 29-32)

**Performance:**
- [ ] Profile and optimize hot paths
- [ ] Tune arena slab sizes
- [ ] Optimize token consumption
- [ ] Benchmark against OCaml compiler parser

**Incremental Parsing:**
- [ ] Implement lookahead tracking
- [ ] Node reuse logic
- [ ] Edit application
- [ ] Benchmarks for incremental edits

**Error Recovery Tuning:**
- [ ] Test on malformed codebases
- [ ] Improve recovery heuristics
- [ ] Add more recovery points

**Diagnostics:**
- [ ] Diagnostic messages
- [ ] Fix-it suggestions
- [ ] Diagnostic filtering

### Phase 11: Tooling (Weeks 33-36)

**Formatter:**
- [ ] Trivia-aware formatting
- [ ] Preserve comments
- [ ] Configurable style

**Linter:**
- [ ] Syntax visitor framework
- [ ] Rule infrastructure
- [ ] Example lint rules

**Codemod Framework:**
- [ ] Syntax rewriting
- [ ] Multi-file transformations
- [ ] Safe refactoring primitives

---

## Key Architectural Decisions

### 1. OCaml or Rust for Implementation?

**Recommendation: Rust**

Reasons:
- Manual memory management required for arena allocator
- Performance-critical (needs to match compiler speed)
- Unsafe pointers for arena-allocated nodes
- Can expose OCaml bindings via `ocaml-rs`

**Alternative: OCaml with C stubs for arena**
- More integrated with ecosystem
- Easier for OCaml community contribution
- Arena allocator in C, rest in OCaml

### 2. Grammar Specification Format

**Recommendation: Declarative DSL in OCaml (like swift-syntax)**

```ocaml
let let_binding = Node.create
  ~kind:`LetBinding
  ~base:`Expr
  ~children:[
    child "LetKeyword" (token `Let);
    child "Pattern" (node `Pattern);
    child "Equals" (token `Equals);
    child "Body" (node `Expr);
  ]
```

**Alternative: External format (YAML/JSON)**
- Requires parser for spec format
- Less type-safe
- But: could be easier for non-OCaml contributors

### 3. Trivia Parsing Strategy

**Recommendation: Lazy trivia parsing (like swift-syntax)**

- Tokens store byte ranges for trivia
- Parse trivia on first access via `arena.parse_trivia`
- Reduces parsing time by ~20% for typical use cases

### 4. OCaml Specifics to Handle

**PPX Attributes:**
```ocaml
type raw_attribute_syntax = {
  at_sign: raw_token;  (* [@, [@@, or [@@@ *)
  name: raw_token;
  payload: raw_attribute_payload option;
  closing: raw_token;  (* ] *)
}
```

**Extension Nodes:**
```ocaml
type raw_extension_syntax = {
  percent: raw_token;  (* % *)
  name: raw_token;
  payload: raw_expr_syntax option;
}
```

**Toplevel Directives:**
```ocaml
type raw_directive_syntax = {
  hash: raw_token;  (* # *)
  name: raw_token;
  arguments: raw_expr_syntax list;
}
```

---

## Testing Strategy

### 1. Round-Trip Testing

**Every input must round-trip:**
```ocaml
let test_round_trip source =
  let tree = parse_source_file source in
  let printed = print_syntax tree in
  assert (source = printed)
```

### 2. Error Recovery Testing

**Malformed inputs must produce structure:**
```ocaml
let test_recovery () =
  let source = "let x = (* missing rhs *)" in
  let tree = parse_source_file source in
  assert (has_error tree);
  assert (is_let_binding tree);
  (* Check body is Missing_expr *)
```

### 3. Incremental Parsing Testing

**Edits should reuse nodes:**
```ocaml
let test_incremental () =
  let old_source = "let x = 1\nlet y = 2" in
  let old_tree = parse_source_file old_source in
  
  let new_source = "let x = 42\nlet y = 2" in
  let edit = { offset = 8; length = 1; replacement = "42" } in
  
  let new_tree = incremental_parse old_tree old_source new_source [edit] in
  (* Assert that 'let y = 2' node was reused *)
```

### 4. Corpus Testing

**Test on real OCaml code:**
- OCaml standard library
- Popular libraries (Core, Lwt, etc.)
- Entire opam repository
- Measure parsing time and memory usage

---

## Performance Targets

Based on swift-syntax performance:

- **Parsing Speed:** 100,000+ lines/second
- **Memory Overhead:** 5-10x source size
- **Incremental Parsing:** Reparse only edited region + some surrounding context
- **Arena Slab Size:** 4096 bytes (tune experimentally)

---

## Conclusion

This architecture provides a solid foundation for building production-quality OCaml tooling:

1. **Source fidelity** enables formatters and refactoring tools
2. **Arena allocation** provides performance competitive with compiler
3. **Error recovery** makes it work on incomplete/invalid code
4. **Incremental parsing** makes it viable for IDE use
5. **Code generation** keeps implementation maintainable

The key insight from swift-syntax is that **every byte counts** - never drop input on the floor, always preserve structure, and make resilience a first-class goal.
