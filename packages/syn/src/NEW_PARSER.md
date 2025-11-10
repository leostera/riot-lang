# The New Parser: Comprehensive Architecture & Wisdom

> **Context:** This document captures all learnings from building the OCaml parser in `syn`, distilling sessions 12-16 of parser development into actionable wisdom for a clean rewrite.

## 🎯 Core Philosophy

### The Three Pillars

1. **Always Lossless**: Every byte of source code must appear in the parse tree
2. **Explicit Trivia Control**: Trivia (whitespace/comments) is NOT allowed everywhere
3. **Always Return Nodes**: Never use `option` types - return ERROR nodes with typed diagnostics

## 📐 Architecture Decisions

### 1. Two Entrypoints (Not File Extension Magic)

```ocaml
(* ✅ GOOD: Explicit entrypoints *)
module Parser : sig
  val parse_ml : string -> node    (* Implementation files *)
  val parse_mli : string -> node   (* Interface files *)
end

(* ❌ BAD: Magic file extension detection *)
val parse : string -> filename:string -> node
```

**Rationale:**
- Caller controls what they're parsing
- No implicit behavior based on filename
- Clearer API surface
- Easier testing (no need for temp files with specific extensions)

### 2. Use TokenCursor Consistently

```ocaml
(* Parser state *)
type parser = {
  cursor : TokenCursor.t;           (* ✅ Use cursor abstraction *)
  diagnostics : Diagnostic.t list;  (* Accumulated parse errors *)
}

(* NOT this manual approach: *)
type bad_parser = {
  tokens : Token.t array;
  position : int;  (* ❌ Manual position tracking *)
}
```

**Rationale:**
- Matches Lexer architecture (uses `Cursor.t` for character stream)
- Consistent API: `peek`, `advance`, `peek_n`, etc.
- Less manual index management
- Fewer off-by-one errors

### 3. Flat Function Structure (Never Nest)

```ocaml
(* ✅ GOOD: Flat mutually recursive structure *)
let rec parse_expr parser = ...
and parse_type_expr parser = ...
and parse_pattern parser = ...
and parse_let_binding parser = ...
and parse_record_field parser = ...
and parse_match_case parser = ...

(* ❌ BAD: Nested helper functions *)
let rec parse_expr parser = 
  let parse_record_field () = (* NOPE! *) in
  let parse_match_case () = (* NOPE! *) in
  ...
```

**Rationale:**
- Easier to navigate (all functions at top level)
- Clear dependency graph
- Better for unit testing individual parsers
- Matches grammar structure 1:1
- No hidden state in closures

## 🔧 Trivia Management: The Critical Issue

### The Problem: Trivia Isn't Allowed Everywhere

**Key Insight:** OCaml's grammar does NOT allow whitespace or comments between certain token pairs!

#### Examples from OCaml Grammar:

```ocaml
(* ✅ VALID *)
'a            (* type variable *)
~name:expr    (* labeled parameter *)
?opt:expr     (* optional parameter *)
List.map      (* field access *)

(* ❌ INVALID - These are syntax errors in OCaml! *)
' (* comment *) a       (* NO space/comment between ' and ident *)
~ (* comment *) name:   (* NO trivia after ~ *)
? (* comment *) opt:    (* NO trivia after ? *)
```

#### From the Grammar (ocaml_grammar.ebnf):

```ebnf
(* Line 184: Type variables are TIGHT *)
typexpr ::= "'" ident  (* No trivia allowed between ' and ident! *)

(* Line 88-90: Labels are TIGHT *)
label ::= "~" label-name ":"
optlabel ::= "?" label-name ":"
```

### The Solution: Explicit Trivia Control

**Do NOT auto-consume trivia after every token!**

```ocaml
(* ❌ BAD: Auto-consumes trivia - loses grammar precision *)
let consume parser =
  let token = get_current_token parser in
  advance parser;
  let trivia = consume_trivia parser in  (* PROBLEM: Not always valid! *)
  (token, trivia)

(* ✅ GOOD: Separate concerns *)
let consume_token_only parser =
  let token = get_current_token parser in
  advance parser;
  token

let consume_trivia parser =
  (* Only call this where grammar explicitly allows trivia! *)
  ...
```

### Where Trivia IS Allowed

Based on the grammar and practical experience:

#### ✅ Trivia IS allowed:
- After keywords: `type (* comment *) foo`
- Between statements/declarations
- Around operators (with care): `x (* a *) + (* b *) y`
- After closing delimiters: `} (* comment *) `
- Before/after commas in lists: `[ 1 (* a *) , (* b *) 2 ]`

#### ❌ Trivia is NOT allowed:
- Between `'` and type variable name: `'a` (atomic)
- Between `~` and label name: `~name` (atomic)
- Between `?` and optional label name: `?opt` (atomic)
- Between `.` and field name in tight contexts (but see below)

### Field Access Special Case

Multi-line field access chains CAN have trivia before the dot:

```ocaml
detail.Server.Module
  .dependencies   (* ✅ Newline + indent before . is valid *)
```

This is because the grammar allows:
```ebnf
expr ::= expr "." field
```

The newline/indent happens BETWEEN expressions, not WITHIN a token sequence.

**Implementation:**
```ocaml
and parse_field_access parser lhs =
  (* Trivia before dot IS allowed (it's between expressions) *)
  let trivia_before_dot = consume_trivia parser in
  let dot = consume_token_only parser in
  (* Trivia after dot IS allowed *)
  let trivia_after_dot = consume_trivia parser in
  let field = consume_token_only parser in
  make_node ~kind:FIELD_ACCESS 
    (trivia_before_dot @ [Node lhs; dot] @ trivia_after_dot @ [field])
```

## 🎨 Always Return Nodes (Never Options)

### The Old Way (BAD)

```ocaml
(* ❌ Problems with option return types *)
val parse_type_decl : parser -> node option

let parse_type_decl parser =
  match peek_kind parser with
  | Some Type -> 
      (* parse successfully *)
      Some node
  | _ -> 
      None  (* 😱 Lost context! No diagnostic! *)
```

**Problems:**
1. No diagnostic about WHY parsing failed
2. Caller has to handle `None` (option pyramid of doom)
3. If trivia was consumed before returning None, it's lost!
4. Not lossless - missing error nodes means missing coverage

### The New Way (GOOD)

```ocaml
(* ✅ Always returns a node *)
val parse_type_decl : parser -> node

let parse_type_decl parser =
  match peek_kind parser with
  | Some Type -> 
      (* parse successfully *)
      node
  | found -> 
      (* Return ERROR node with typed diagnostic *)
      make_error_node parser
        ~kind:(UnexpectedToken { 
          expected = "type keyword"; 
          found = describe_token found 
        })
        ~span:(current_span parser)
```

**Benefits:**
1. ✅ Always lossless - ERROR nodes capture all trivia
2. ✅ Typed diagnostics (not strings!)
3. ✅ Simpler API - no option handling
4. ✅ Better error messages for users
5. ✅ Easier to compose parsers

### Typed Diagnostics

```ocaml
(* packages/syn/src/diagnostic.ml *)
type t =
  | UnexpectedToken of { expected : string; found : string }
  | ExpectedAfter of { after : string; expected : string; found : string }
  | MissingDelimiter of { delimiter : string; context : string }
  | InvalidTrivia of { location : string; reason : string }
  | UnclosedDelimiter of { delimiter : string; opened_at : span }
  (* Add more as needed *)

(* NOT this: *)
type bad_diagnostic = string  (* ❌ Untyped, hard to pattern match *)
```

**Why typed errors?**
- Pattern matching on error types
- Structured error reporting
- IDE integration (show specific fixes)
- Catch error construction bugs at compile time

## 📚 Grammar-Driven Structure

### One Function Per Grammar Rule

**Every non-terminal in the EBNF = one `parse_*` function**

```ebnf
(* From ocaml_grammar.ebnf *)

(* 1. TYPE EXPRESSIONS *)
typexpr ::=
    "'" ident
  | "_"
  | "(" typexpr ")"
  | [["?"]label-name":"] typexpr "->" typexpr
  | typexpr { "*" typexpr }+
  | typeconstr
  | typexpr typeconstr
  | "(" typexpr { "," typexpr } ")" typeconstr
  | typexpr "as" "'" ident
  | polymorphic-variant-type
  | ...
```

**Maps to:**

```ocaml
and parse_typexpr parser =
  match peek_kind parser with
  | Some Quote -> parse_type_variable parser
  | Some Underscore -> parse_wildcard_type parser
  | Some (OpenDelim Paren) -> parse_paren_or_tuple_type parser
  | Some (Ident _) -> parse_typeconstr parser
  | Some (OpenDelim Bracket) -> parse_polymorphic_variant_type parser
  | found -> 
      make_error_node parser
        ~kind:(Expected { expected = "type expression"; found })

and parse_type_variable parser =
  (* "'" ident - NO TRIVIA between tokens! *)
  match peek_kind parser with
  | Some Quote ->
      let quote = consume_token_only parser in
      (* IMMEDIATELY get ident, no trivia! *)
      match peek_kind parser with
      | Some (Ident name) ->
          let ident = consume_token_only parser in
          make_node ~kind:TYPE_VAR [quote; ident]
      | found ->
          make_error_node parser
            ~kind:(ExpectedAfter { 
              after = "quote"; 
              expected = "identifier"; 
              found 
            })
  | _ -> panic "unreachable"

and parse_wildcard_type parser = ...
and parse_paren_or_tuple_type parser = ...
and parse_typeconstr parser = ...
and parse_polymorphic_variant_type parser = ...
```

### Complete Grammar Coverage Checklist

For each section of `ocaml_grammar.ebnf`, ensure there's a corresponding parser:

#### ✅ Type Expressions (Section 4)
- [ ] `parse_typexpr` (main dispatcher)
- [ ] `parse_type_variable` (`'a`)
- [ ] `parse_wildcard_type` (`_`)
- [ ] `parse_paren_type` (`(typexpr)`)
- [ ] `parse_arrow_type` (`t1 -> t2`)
- [ ] `parse_tuple_type` (`t1 * t2 * t3`)
- [ ] `parse_typeconstr` (type constructor application)
- [ ] `parse_polymorphic_variant_type` (`[> `Foo | `Bar]`)
- [ ] `parse_object_type` (`< m : t >`)
- [ ] `parse_poly_typexpr` (for `val` declarations)

#### ✅ Patterns (Section 6)
- [ ] `parse_pattern` (main dispatcher)
- [ ] `parse_value_name_pattern`
- [ ] `parse_wildcard_pattern`
- [ ] `parse_constant_pattern`
- [ ] `parse_as_pattern`
- [ ] `parse_or_pattern` (`p1 | p2`)
- [ ] `parse_constructor_pattern`
- [ ] `parse_tuple_pattern`
- [ ] `parse_record_pattern`
- [ ] `parse_list_pattern`
- [ ] `parse_array_pattern`
- [ ] `parse_lazy_pattern`
- [ ] `parse_exception_pattern`

#### ✅ Expressions (Section 7)
- [ ] `parse_expr` (main dispatcher with precedence)
- [ ] `parse_value_path_expr`
- [ ] `parse_constant_expr`
- [ ] `parse_paren_expr`
- [ ] `parse_tuple_expr`
- [ ] `parse_constructor_app_expr`
- [ ] `parse_record_expr`
- [ ] `parse_record_update_expr`
- [ ] `parse_function_app_expr`
- [ ] `parse_field_access_expr`
- [ ] `parse_array_access_expr`
- [ ] `parse_if_expr`
- [ ] `parse_match_expr`
- [ ] `parse_function_expr`
- [ ] `parse_fun_expr`
- [ ] `parse_let_expr`
- [ ] `parse_sequence_expr` (`;`)
- [ ] `parse_try_expr`
- [ ] `parse_while_expr`
- [ ] `parse_for_expr`
- [ ] `parse_local_open_expr`

#### ✅ Type Definitions (Section 8)
- [ ] `parse_type_definition`
- [ ] `parse_typedef`
- [ ] `parse_type_params`
- [ ] `parse_type_param` (with variance)
- [ ] `parse_record_decl`
- [ ] `parse_constr_decl`
- [ ] `parse_field_decl`
- [ ] `parse_type_constraint`
- [ ] `parse_exception_definition`

#### ✅ Module System
- [ ] `parse_module_expr`
- [ ] `parse_module_type`
- [ ] `parse_signature_item`
- [ ] `parse_structure_item`
- [ ] `parse_val_decl` (interface files)
- [ ] `parse_module_decl`
- [ ] `parse_functor`

## 🧪 Test-Driven Development

### The Testing Strategy

**Every `parse_*` function must have tests BEFORE implementation!**

Use the new `Std.Test` and `Std.Test.Cli` modules:

```ocaml
(* packages/syn/tests/test_type_variable.ml *)
open Std
open Std.Test

let test_basic_type_var () =
  let source = "'a" in
  let tokens = Lexer.lex source in
  let parser = Parser.create tokens in
  let node = Parser.parse_type_variable parser in
  
  assert_equal 
    ~msg:"Type variable should parse completely"
    ~expected:2 
    ~actual:(node_width node);
  
  assert_equal
    ~msg:"Should be TYPE_VAR node"
    ~expected:Syntax.TYPE_VAR
    ~actual:(node_kind node)

let test_type_var_rejects_space () =
  let source = "' a" in  (* Invalid! *)
  let tokens = Lexer.lex source in
  let parser = Parser.create tokens in
  let node = Parser.parse_type_variable parser in
  
  assert_true
    ~msg:"Should return ERROR node for space after quote"
    (is_error_node node);
  
  match get_diagnostic node with
  | Some (ExpectedAfter { after = "quote"; expected = "identifier"; _ }) -> ()
  | _ -> fail "Wrong diagnostic type"

let test_type_var_with_underscore () =
  let source = "'_foo" in
  let tokens = Lexer.lex source in
  let parser = Parser.create tokens in
  let node = Parser.parse_type_variable parser in
  
  assert_equal ~expected:5 ~actual:(node_width node)

let test_type_var_with_trailing_prime () =
  let source = "'a'" in  (* Single quote at end of ident is valid *)
  let tokens = Lexer.lex source in
  let parser = Parser.create tokens in
  let node = Parser.parse_type_variable parser in
  
  assert_equal ~expected:3 ~actual:(node_width node)

let () =
  Test.Cli.run [
    test "basic type variable" test_basic_type_var;
    test "rejects space between quote and ident" test_type_var_rejects_space;
    test "accepts underscore in ident" test_type_var_with_underscore;
    test "accepts trailing prime in ident" test_type_var_with_trailing_prime;
  ]
```

### Test Categories for Each Parser

For every `parse_*` function, write tests for:

1. **Happy Path**: Minimal valid input
2. **With Trivia**: Input with allowed whitespace/comments
3. **Error Cases**: Invalid trivia placement
4. **Edge Cases**: Optional parts, empty lists, etc.
5. **Grammar Variations**: All alternatives from EBNF

### Coverage Verification

After implementing a parser, use `debug_trivia.py`:

```bash
./packages/syn/tests/debug_trivia.py test_file.ml
```

Should show:
```
Source file length: 42 bytes
Red tree coverage: 42/42 bytes
✓ Complete coverage!
```

## 🛠️ Parser Helper Functions

### Essential Helpers

```ocaml
(* Token inspection *)
val peek : parser -> Token.t option
val peek_kind : parser -> Token.kind option
val peek_n : parser -> int -> Token.t option
val at : parser -> Token.kind -> bool
val is_eof : parser -> bool

(* Token consumption *)
val consume_token_only : parser -> Token.t
val consume_trivia : parser -> Token.t list

(* Expecting specific tokens *)
val expect : parser -> Token.kind -> Token.t
  (* Raises error if not found - use for required tokens *)

val try_expect : parser -> Token.kind -> Token.t option
  (* Returns None if not found - use for optional tokens *)

(* Error handling *)
val make_error_node : parser -> kind:Diagnostic.t -> span:span -> node
val report_diagnostic : parser -> Diagnostic.t -> unit
val current_span : parser -> span

(* Node construction *)
val make_node : kind:Syntax.t -> Token.t list -> node
val make_node_with_children : kind:Syntax.t -> node list -> node
```

### Delimiter Helpers

```ocaml
(* Handle paired delimiters with proper trivia *)
val parse_delimited :
  parser ->
  open_delim:Token.kind ->
  close_delim:Token.kind ->
  parse_inner:(parser -> node) ->
  node

(* Example usage: *)
let parse_paren_type parser =
  parse_delimited parser
    ~open_delim:(OpenDelim Paren)
    ~close_delim:(CloseDelim Paren)
    ~parse_inner:parse_typexpr
```

## 📖 Debugging Strategy

### When Coverage is Missing

Use the systematic approach from sessions 12-16:

#### 1. Verify the Issue
```bash
./packages/syn/tests/debug_trivia.py problematic_file.ml
```

Output shows:
```
Source file length: 1478 bytes
Red tree coverage: 1450/1478 bytes
✗ Missing 28 bytes

Last token in green tree before gap: Kind: WHITESPACE Span: [1449..1450]
```

#### 2. Binary Search
```bash
head -n 50 problematic_file.ml > test.ml
./packages/syn/tests/debug_trivia.py test.ml
# Adjust line count until you find the problematic section
```

#### 3. Create Minimal Test Case
```bash
# Extract just the problematic pattern
echo "detail.Server.Module
  .dependencies" > minimal.ml
./packages/syn/tests/debug_trivia.py minimal.ml
```

#### 4. Identify the Pattern
Common missing trivia patterns:
- Trivia before closing delimiters
- Trivia between field access chains
- Trivia after semicolons in lists
- Trivia in record expressions
- Leading trivia consumed but not added to tree

#### 5. Fix the Parser
Usually one of:
- Add missing `trivia_before_X` to children
- Pass `leading_trivia` as parameter instead of consuming internally
- Fix accumulator order in list builders
- Add trivia to ERROR nodes

#### 6. Verify Fix
```bash
./packages/syn/tests/debug_trivia.py problematic_file.ml
# Should show 100% coverage
```

## 🎯 Common Pitfalls & Solutions

### Pitfall 1: Double Trivia Consumption

**Problem:**
```ocaml
(* ❌ BAD *)
let parse_record_field parser =
  let leading_trivia = consume_trivia parser in  (* Consume here *)
  ...
  
let parse_record_expr parser =
  ...
  let trivia = consume_trivia parser in  (* And here! *)
  match parse_record_field parser with
  | field -> ...  (* trivia lost! *)
```

**Solution:**
```ocaml
(* ✅ GOOD *)
let parse_record_field parser leading_trivia =
  (* Take trivia as parameter, don't consume internally *)
  ...
  
let parse_record_expr parser =
  ...
  let trivia = consume_trivia parser in
  match parse_record_field parser trivia with
  | field -> ...  (* trivia passed along *)
```

### Pitfall 2: Wrong Accumulator Order

**Problem:**
```ocaml
(* ❌ BAD - builds list in reverse *)
let rec parse_list acc =
  let node = parse_item parser in
  let trivia = consume_trivia parser in
  parse_list (node :: trivia @ acc)  (* WRONG ORDER *)
  
(* After List.rev: [trivia1; node1; trivia2; node2] - trivia before nodes! *)
```

**Solution:**
```ocaml
(* ✅ GOOD - append to end *)
let rec parse_list acc =
  let node = parse_item parser in
  let trivia = consume_trivia parser in
  parse_list (acc @ [node] @ trivia)  (* Correct order maintained *)
  
(* No rev needed: [node1; trivia1; node2; trivia2] *)
```

Or use a proper data structure:
```ocaml
(* ✅ BETTER - use Queue for efficient append *)
let rec parse_list queue =
  let node = parse_item parser in
  let trivia = consume_trivia parser in
  Queue.push node queue;
  List.iter (Queue.push queue) trivia;
  parse_list queue
```

### Pitfall 3: Not Including Trivia in ERROR Nodes

**Problem:**
```ocaml
(* ❌ BAD - trivia lost on error *)
let parse_type_decl parser =
  let leading_trivia = consume_trivia parser in
  match peek_kind parser with
  | Some Type -> (* parse *)
  | found ->
      (* trivia consumed but not in error node! *)
      make_error_node parser ~kind:(Expected { expected = "type"; found })
```

**Solution:**
```ocaml
(* ✅ GOOD - include trivia in error *)
let parse_type_decl parser =
  let leading_trivia = consume_trivia parser in
  match peek_kind parser with
  | Some Type -> (* parse *)
  | found ->
      make_error_node parser 
        ~kind:(Expected { expected = "type"; found })
        ~trivia:leading_trivia  (* Include consumed trivia! *)
```

### Pitfall 4: Forgetting Trivia After Closing Delimiters

**Problem:**
```ocaml
(* ❌ BAD - missing trivia after close *)
let parse_paren_expr parser =
  let open_paren = expect parser (OpenDelim Paren) in
  let trivia_after_open = consume_trivia parser in
  let inner = parse_expr parser in
  let close_paren = expect parser (CloseDelim Paren) in
  (* Missing: trivia after close_paren! *)
  make_node ~kind:PAREN_EXPR 
    [open_paren] @ trivia_after_open @ [Node inner; close_paren]
```

**Solution:**
```ocaml
(* ✅ GOOD - include trivia after close *)
let parse_paren_expr parser =
  let open_paren = expect parser (OpenDelim Paren) in
  let trivia_after_open = consume_trivia parser in
  let inner = parse_expr parser in
  let close_paren = expect parser (CloseDelim Paren) in
  let trivia_after_close = consume_trivia parser in  (* Don't forget! *)
  make_node ~kind:PAREN_EXPR 
    ([open_paren] @ trivia_after_open @ [Node inner; close_paren] 
     @ trivia_after_close)
```

## 🚀 Implementation Roadmap

### Phase 1: Foundation (Week 1)
1. ✅ Create `TokenCursor.t` module
2. Create new `parser.ml` with clean architecture:
   - Use `TokenCursor.t`
   - Typed `Diagnostic.t` variants
   - No `option` return types
3. Implement core helpers:
   - `peek`, `at`, `consume_token_only`, `consume_trivia`
   - `expect`, `make_error_node`, `make_node`
4. Add `parse_ml` and `parse_mli` entrypoints

### Phase 2: Type System (Week 2)
1. Implement all type expression parsers
2. Write comprehensive tests for each
3. Verify 100% coverage with `debug_trivia.py`
4. Document trivia rules for type expressions

### Phase 3: Patterns (Week 3)
1. Implement all pattern parsers
2. Tests for each pattern variant
3. Handle precedence correctly (tuple, or, etc.)

### Phase 4: Expressions (Week 4-5)
1. Implement expression parsers with precedence
2. Handle all operators correctly
3. Special attention to:
   - Field access chains
   - Function application
   - Record expressions
   - Let bindings

### Phase 5: Declarations (Week 6)
1. Type definitions
2. Module system
3. Exception definitions
4. Class system (if needed)

### Phase 6: Top Level (Week 7)
1. Structure items (.ml files)
2. Signature items (.mli files)
3. Complete integration tests

### Phase 7: Migration (Week 8)
1. Run on entire codebase
2. Compare with old parser
3. Fix any regressions
4. Update all tooling to use new parser

## 📊 Success Metrics

### Parser Quality Metrics

1. **Coverage**: `debug_trivia.py` shows 100% on all test files
2. **Error Quality**: All errors are typed `Diagnostic.t` variants
3. **Performance**: Parse large files (10k+ lines) in < 100ms
4. **Maintainability**: Every grammar rule has 1:1 function mapping

### Testing Metrics

1. **Unit Tests**: Every `parse_*` function has ≥4 tests
2. **Integration Tests**: Full files parse to 100% coverage
3. **Error Tests**: Invalid syntax produces meaningful diagnostics
4. **Regression Tests**: Old bug cases remain fixed

## 🎓 Key Learnings Summary

1. **Trivia is NOT Allowed Everywhere**
   - Type variables: `'a` is atomic (no space)
   - Labels: `~name:` is tight
   - Consult grammar to know where trivia is permitted

2. **Always Return Nodes**
   - ERROR nodes with typed diagnostics
   - No `option` pyramid of doom
   - Lossless even on syntax errors

3. **Flat Function Structure**
   - One function per grammar rule
   - All mutually recursive at top level
   - Easy to navigate and test

4. **TDD Everything**
   - Write tests before implementation
   - Use `debug_trivia.py` to verify coverage
   - Test happy path + errors + edge cases

5. **Trust Tusk's Cache**
   - If it says "cached", it IS cached
   - Don't fight the build system
   - Focus on correctness, not rebuild tricks

## 📚 References

- OCaml Grammar: `packages/syn/docs/ocaml_grammar.ebnf`
- Session Logs: `llm/sessions/claude-building-syn-part-{12,13,14,15,16}.md`
- Test Framework: `packages/std/src/test/`
- Coverage Tool: `packages/syn/tests/debug_trivia.py`

---

**Remember:** A lossless parser is a correct parser. Every byte matters! 🎯
