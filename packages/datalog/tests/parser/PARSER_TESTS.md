# Datalog Parser Test Fixtures

## Overview

Parser tests focus on **syntax correctness** and **AST generation**, separate from runtime evaluation. We use `ceibo` for lossless syntax tree parsing (like `syn` in Rust) with excellent diagnostics.

## Test Categories

### 1. Valid Syntax (parser/valid/)
Tests that should parse successfully and produce correct AST

### 2. Invalid Syntax (parser/invalid/)
Tests that should fail with helpful error messages

### 3. Edge Cases (parser/edge/)
Boundary conditions, whitespace handling, etc.

### 4. Error Recovery (parser/recovery/)
How parser handles and reports multiple errors

## Fixture Format

### Valid Parse Test
```
fixtures/valid/0001_simple_fact.datalog
fixtures/valid/0001_simple_fact.ast.json
```

**Input (.datalog):**
```datalog
person("alice").
```

**Expected AST (.ast.json):**
```json
{
  "type": "program",
  "items": [
    {
      "type": "fact",
      "predicate": {
        "type": "identifier",
        "value": "person",
        "span": {"start": 0, "end": 6}
      },
      "args": [
        {
          "type": "string_literal",
          "value": "alice",
          "span": {"start": 7, "end": 14}
        }
      ],
      "span": {"start": 0, "end": 16}
    }
  ]
}
```

### Invalid Parse Test
```
fixtures/invalid/0001_missing_paren.datalog
fixtures/invalid/0001_missing_paren.error.json
```

**Input (.datalog):**
```datalog
person("alice".
```

**Expected Error (.error.json):**
```json
{
  "errors": [
    {
      "type": "syntax_error",
      "message": "Expected ')' to close argument list",
      "span": {"start": 14, "end": 15},
      "help": "Add closing parenthesis here",
      "severity": "error"
    }
  ]
}
```

## Parser Feature Coverage

### Basic Syntax
- [ ] Facts (unary, binary, ternary, etc.)
- [ ] Rules (head :- body)
- [ ] Variables (X, Y, Name, etc.)
- [ ] Constants (strings, integers, atoms)
- [ ] Wildcards (_)
- [ ] Comments (% and %%)

### Complex Syntax
- [ ] Negation (!pred)
- [ ] Built-in predicates (>, <, =, !=)
- [ ] Multiple rules
- [ ] Recursive rules
- [ ] Long predicate names
- [ ] Unicode identifiers

### Whitespace & Formatting
- [ ] Leading/trailing whitespace
- [ ] Multiple spaces
- [ ] Tabs vs spaces
- [ ] Empty lines
- [ ] Inline comments
- [ ] Multi-line rules

### Error Cases
- [ ] Missing punctuation (., ), (, :-, etc.)
- [ ] Invalid identifiers
- [ ] Unclosed strings
- [ ] Invalid escape sequences
- [ ] Mismatched parentheses
- [ ] Invalid variable names (lowercase start)
- [ ] Empty predicates
- [ ] Malformed rules

### Recovery Cases
- [ ] Multiple errors in single file
- [ ] Error in first line
- [ ] Error in last line
- [ ] Cascading errors
- [ ] Recover after error and continue

## Lossless Parsing with Ceibo

Key features we want (like `syn`):

1. **Full Fidelity**: Preserve all whitespace, comments, formatting
2. **Span Information**: Byte offsets for every token
3. **Error Recovery**: Continue parsing after errors
4. **Helpful Diagnostics**: Point to exact error location with context
5. **AST Transformation**: Easy to traverse and transform

## Example: Good Error Message

```
error: expected closing parenthesis
  ┌─ test.datalog:3:15
  │
3 │ person("alice".
  │               ^ help: add `)` here
  │
  = note: argument lists must be properly closed
```

## Implementation Plan

1. **Lexer** (`src/parser/lexer.ml`)
   - Tokenize with span information
   - Handle comments
   - Preserve whitespace in CST

2. **Parser** (`src/parser/parser.ml`)
   - Build CST using `ceibo`
   - Recursive descent with error recovery
   - Rich error reporting

3. **AST** (`src/parser/ast.ml`)
   - Lower CST to AST
   - Validate semantic rules
   - Type-safe representation

4. **Diagnostics** (`src/parser/diagnostic.ml`)
   - Error formatting
   - Span utilities
   - Colorized output

## Testing Approach

```ocaml
let test_valid_parse file =
  let input = read_file (file ^ ".datalog") in
  let expected_ast = Json.from_file (file ^ ".ast.json") in
  
  match Parser.parse input with
  | Ok ast ->
      assert_equal expected_ast (Ast.to_json ast)
  | Error errors ->
      failwith (sprintf "Parse failed: %s" (Diagnostic.format errors))

let test_invalid_parse file =
  let input = read_file (file ^ ".datalog") in
  let expected_errors = Json.from_file (file ^ ".error.json") in
  
  match Parser.parse input with
  | Ok _ ->
      failwith "Expected parse to fail, but it succeeded"
  | Error errors ->
      assert_equal expected_errors (Diagnostic.to_json errors)
```

## Comparison: Parser vs Runtime Tests

| Aspect | Parser Tests | Runtime Tests |
|--------|-------------|---------------|
| **Focus** | Syntax correctness | Semantic evaluation |
| **Input** | Raw Datalog text | Parsed AST |
| **Output** | AST or errors | Query results |
| **Count** | ~200 fixtures | ~500 fixtures |
| **Errors** | Syntax errors | Logic errors |
| **Tools** | Ceibo (CST) | Evaluator |

Both test suites are essential for a robust implementation!
