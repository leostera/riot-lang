# syn

A lossless OCaml parser producing a Concrete Syntax Tree (CST) for developer tooling.

## Purpose

Parse OCaml source code into a **lossless syntax tree** that preserves:
- All tokens (keywords, operators, literals, delimiters)
- All trivia (comments, docstrings, whitespace positions)
- Exact source structure and formatting

Perfect for building:
- 🎨 Code formatters
- 🔍 Linters and analyzers  
- 🛠️ Refactoring tools
- 📝 Documentation generators
- 🔬 Static analysis tools

**Not** for compilation - for tooling!

## Architecture

```
Source Code
    ↓
  Lexer → Token stream (with positions)
    ↓
  Parser → Lossless CST (Ceibo Green Tree)
    ↓
  Red Tree → Traversable AST with parent pointers
```

## Key Features

### 1. Lossless Parsing
Every byte of the source is represented in the tree:
- All whitespace positions tracked
- Comments and docstrings preserved
- Exact formatting recoverable

### 2. Ceibo Green/Red Trees
- **Green Tree**: Immutable, memory-efficient, shareable
- **Red Tree**: Lazy wrapper with parent pointers for traversal
- Convert between them as needed

### 3. Production Ready
- ✅ **100% codebase coverage** (443/443 files in Riot monorepo)
- ✅ **98.9% test coverage** (1081/1093 tests)
- ✅ Handles all modern OCaml syntax
- ✅ Private types, modules, functors, objects, polymorphic variants
- ✅ Labeled/optional arguments, GADTs, first-class modules

## API

### Command Line

```bash
# Parse a file and output JSON
tusk run syn -- parse --json file.ml

# Tokenize a file
tusk run syn -- token-stream --json file.ml
```

### Library

```ocaml
open Std

(* Parse source code *)
let source = Fs.read (Path.v "file.ml") |> Result.unwrap in
let tokens = Syn.Lexer.tokenize source in
let tree = Syn.Parser.parse tokens in

match tree with
| Ok green_tree ->
    (* Work with the lossless CST *)
    let width = Ceibo.Green.width green_tree in
    Log.info "Parsed %d bytes" width
    
| Error diagnostics ->
    List.iter (fun d -> 
      Log.error "Parse error: %s" (Syn.Diagnostic.to_string d)
    ) diagnostics
```

## CST Structure

The parser produces a hierarchical tree of nodes and tokens:

```ocaml
type ('kind, 'data) element =
  | Node of ('kind, 'data) node
  | Token of 'data

type ('kind, 'data) node = {
  kind : 'kind;
  width : int;
  children : ('kind, 'data) element list;
}
```

Each node has a **syntax kind** (e.g., `LET_BINDING`, `IF_EXPR`, `TYPE_DECL`) and contains:
- Child nodes (sub-structures)
- Tokens (keywords, identifiers, operators)
- Trivia (comments, docstrings)

## Example

```ocaml
(* Source: *)
let add x y = x + y

(* CST structure: *)
SOURCE_FILE
└─ LET_BINDING
   ├─ 'let' keyword
   ├─ IDENT_PATTERN (name: 'add')
   ├─ IDENT_PATTERN (param: 'x')
   ├─ IDENT_PATTERN (param: 'y')
   ├─ '=' token
   └─ INFIX_EXPR
      ├─ IDENT_EXPR (name: 'x')
      ├─ '+' operator
      └─ IDENT_EXPR (name: 'y')
```

## Supported Syntax

### Expressions ✅
- Literals, identifiers, operators
- Function application and definition
- Let bindings (let, let rec, let module)
- Pattern matching (match, function, try)
- Conditionals (if/then/else)
- Sequences and blocks
- Records, tuples, lists, arrays
- Objects and polymorphic variants

### Patterns ✅
- Variable, constant, wildcard patterns
- Constructor and tuple patterns
- Record and array patterns  
- Or-patterns and guards
- As-patterns and type annotations

### Types ✅
- Type constructors and variables
- Function types (arrows)
- Tuple and record types
- Variant and polymorphic variant types
- Object types
- Module types and signatures
- GADTs and constraints

### Declarations ✅
- Type declarations (normal, private, extensible)
- Value declarations (val, external)
- Module declarations and signatures
- Module type declarations
- Class and class type declarations
- Exception declarations

### Modules ✅
- Module expressions (struct...end)
- Module types (sig...end)
- Functors and applications
- Module constraints and sealing
- First-class modules

## Design Philosophy

**Lossless over Convenient**:
- Preserve every detail of the source
- Enable accurate formatting and refactoring
- Comments in the right places

**Structure over Semantics**:
- Syntax tree, not semantic tree
- No name resolution or type checking
- Fast and simple

**Production Ready**:
- Handles real-world code
- Comprehensive test coverage
- Battle-tested on large codebase

## Non-Goals

- ❌ Not a compiler (use OCaml's compiler-libs)
- ❌ Not desugaring (preserves exact syntax)
- ❌ Not semantic analysis (just syntax)
- ❌ Not for learning OCaml (use tutorials)

## Status

🎉 **Production Ready**

Coverage:
- ✅ Lexer: Complete
- ✅ Parser: 100% codebase coverage
- ✅ Tests: 98.9% (1081/1093)
- ✅ Trivia: Fully preserved

Used by:
- **tusk_fix**: OCaml linter
- **tusk_fmt**: Code formatter (planned)

## Contributing

The parser is structured as a recursive-descent Pratt parser:
- `lexer.ml`: Tokenization with position tracking
- `parser.ml`: Recursive descent parsing
- `ceibo/`: Green/Red tree implementation
- `syntax_kind.ml`: All node types
- `diagnostic.ml`: Error reporting

To add support for new syntax:
1. Add tokens to lexer if needed
2. Add syntax kind to `syntax_kind.ml`
3. Add parsing function to `parser.ml`
4. Add tests to `tests/fixtures/`
5. Run `./packages/syn/tests/regenerate_expected.sh`

See existing parsing functions for patterns to follow.

## License

Same as Riot project
