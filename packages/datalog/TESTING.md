# Datalog Testing Strategy

## Overview

We maintain two separate test suites with clear separation of concerns:

1. **Parser Tests** (~150 fixtures): Syntax correctness, AST generation, error handling
2. **Runtime Tests** (~500 fixtures): Semantic evaluation, query results

## Directory Structure

```
packages/datalog/tests/
├── parser/              # Parser test fixtures
│   ├── fixtures/
│   │   ├── valid/       # Valid syntax → AST
│   │   ├── invalid/     # Syntax errors → Error messages
│   │   └── edge/        # Edge cases & boundary conditions
│   ├── PARSER_TESTS.md
│   └── generate_parser_fixtures.py
│
└── runtime/             # Runtime evaluation tests
    └── fixtures/        # 500 comprehensive test cases
        ├── 0001-0050: Basic facts
        ├── 0051-0100: Simple queries
        ├── 0101-0150: Joins
        ├── 0151-0200: Rules
        ├── 0201-0250: Recursion
        ├── 0251-0300: Negation
        ├── 0301-0350: Built-ins
        ├── 0351-0400: Complex queries
        ├── 0401-0450: Graph algorithms
        ├── 0451-0502: Real-world scenarios
        ├── FIXTURES.md
        └── README.md
```

## Parser Tests (tests/parser/)

### Purpose
Test that the **parser correctly handles Datalog syntax** and produces accurate ASTs or helpful error messages.

### Test Format

**Valid Parse:**
```
fixtures/valid/0001_simple_fact.datalog
fixtures/valid/0001_simple_fact.ast.json
```

**Invalid Parse:**
```
fixtures/invalid/0001_missing_paren.datalog
fixtures/invalid/0001_missing_paren.error.json
```

### Coverage (150 tests)

- ✅ **Valid Syntax** (~80 tests)
  - Facts (all arities)
  - Rules (simple, complex, recursive)
  - Variables, constants, wildcards
  - Negation, built-ins
  - Comments

- ✅ **Invalid Syntax** (~50 tests)
  - Missing punctuation
  - Unclosed strings/parens
  - Invalid identifiers
  - Malformed rules
  - Type errors

- ✅ **Edge Cases** (~20 tests)
  - Whitespace handling
  - Unicode support
  - Long identifiers
  - Empty programs
  - Comment variations

### Parser Features (Ceibo-based)

Like Rust's `syn`, we want:
- **Lossless parsing**: Preserve all formatting, whitespace, comments
- **Span tracking**: Byte-level position for every token
- **Error recovery**: Continue parsing after errors
- **Rich diagnostics**: Beautiful error messages with context

Example error output:
```
error: expected closing parenthesis
  ┌─ test.datalog:3:15
  │
3 │ person("alice".
  │               ^ help: add `)` here
  │
  = note: argument lists must be properly closed
```

## Runtime Tests (tests/runtime/)

### Purpose
Test that the **evaluator correctly computes query results** according to Datalog semantics.

### Test Format

```
fixtures/0010_transitive_closure.datalog
fixtures/0010_transitive_closure.datalog.expected
```

**Input (.datalog):**
```datalog
edge(1, 2).
edge(2, 3).
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

**Expected Output (.expected):**
```json
{
  "facts": [...],
  "query": "path(X, Y)",
  "result": [
    {"X": 1, "Y": 2},
    {"X": 1, "Y": 3},
    {"X": 2, "Y": 3}
  ]
}
```

### Coverage (500 tests)

- ✅ **Basic Facts** (50): Types, arities, edge cases
- ✅ **Queries** (50): Pattern matching, variables, constants
- ✅ **Joins** (50): 2-way, 3-way, 4-way, self-joins
- ✅ **Rules** (50): Derivation, chaining, constants
- ✅ **Recursion** (50): Transitive closure, deep/wide recursion
- ✅ **Negation** (50): Stratified, complements, interaction
- ✅ **Built-ins** (50): Comparisons, equality, ranges
- ✅ **Complex** (50): Multi-rule, long chains, combined features
- ✅ **Graphs** (50): Reachability, cycles, components
- ✅ **Real-world** (50): Practical scenarios

## Testing Workflow

### 1. Development Cycle

```bash
# Test parser on a single file
./test_parser tests/parser/fixtures/valid/0001_simple_fact.datalog

# Test runtime on a single fixture
./test_runtime tests/runtime/fixtures/0010_transitive_closure.datalog

# Test a category
./test_runtime_category 0201-0250  # All recursion tests

# Test everything
./test_all
```

### 2. Continuous Integration

```yaml
# .github/workflows/test.yml
test:
  - name: Parser Tests
    run: ./run_parser_tests
  
  - name: Runtime Tests (Basic)
    run: ./run_runtime_tests 0001-0100
  
  - name: Runtime Tests (Advanced)
    run: ./run_runtime_tests 0101-0502
```

### 3. TDD Workflow

1. **Start with parser tests** - Get syntax right first
2. **Move to runtime tests** - Implement evaluation
3. **Iterate** - Fix bugs revealed by tests
4. **Add tests** - When bugs are found, add regression tests

## Implementation Order

### Phase 1: Parser (Week 1)
1. Implement lexer
2. Pass valid syntax tests (80)
3. Pass invalid syntax tests (50)
4. Pass edge case tests (20)
5. Polish error messages

### Phase 2: Core Runtime (Week 2)
1. Basic facts (0001-0050)
2. Simple queries (0051-0100)
3. Simple joins (0101-0150)
4. Simple rules (0151-0200)

### Phase 3: Advanced Runtime (Week 3)
1. Recursion (0201-0250)
2. Negation (0251-0300)
3. Built-ins (0301-0350)
4. Complex queries (0351-0400)

### Phase 4: Real-World (Week 4)
1. Graph algorithms (0401-0450)
2. Real-world scenarios (0451-0502)
3. Performance optimization
4. Documentation

## Test Metrics

### Parser
- **Syntax Coverage**: 100% of grammar
- **Error Coverage**: All error types
- **Edge Cases**: All boundary conditions
- **Total**: 150 tests

### Runtime
- **Feature Coverage**: All Datalog features
- **Complexity**: Simple → Complex progression
- **Real-world**: Practical scenarios
- **Total**: 500 tests

### Combined
- **Total Tests**: 650
- **Expected Pass Rate**: 100%
- **Execution Time**: < 10 seconds (all tests)

## Test Quality Standards

### Good Test Characteristics
✅ **Focused**: Tests one thing
✅ **Named**: Clear what it tests
✅ **Documented**: Comments explain why
✅ **Fast**: Runs in milliseconds
✅ **Deterministic**: Always same result
✅ **Independent**: No dependencies on other tests

### Bad Test Characteristics
❌ Tests multiple things
❌ Generic names like "test1"
❌ No explanation
❌ Slow execution
❌ Flaky results
❌ Depends on test order

## Maintenance

### Adding New Tests

**Parser Test:**
```bash
cd tests/parser
# Add to generate_parser_fixtures.py
python3 generate_parser_fixtures.py
```

**Runtime Test:**
```bash
cd tests/runtime/fixtures
# Create NNNN_name.datalog
# Create NNNN_name.datalog.expected
```

### Updating Tests

When semantics change:
1. Update affected `.expected` files
2. Document why in commit message
3. Ensure backwards compatibility

### Removing Tests

Only remove tests if:
1. Feature is deprecated
2. Test is duplicate
3. Test is incorrect

Document reason in commit.

## References

- **Parser Design**: Inspired by Rust's `syn` and `rowan`
- **Runtime Tests**: Based on Datafrog, Crepe, DataScript test suites
- **Error Messages**: Following Rust compiler quality standards
- **Test Coverage**: Aiming for 100% feature coverage

---

**Status**: ✅ Test infrastructure complete (650 fixtures)
**Next**: Implement parser and evaluator to make tests pass!
