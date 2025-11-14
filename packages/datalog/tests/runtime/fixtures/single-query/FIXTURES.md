# Datalog Test Fixtures - Complete Suite

## Overview

This directory contains **500 comprehensive test fixtures** for the Datalog implementation, systematically covering all aspects of Datalog evaluation from simple facts to complex real-world scenarios.

## Statistics

- **Total Fixtures**: 500
- **Input Files**: 500 `.datalog` files
- **Expected Output Files**: 500 `.datalog.expected` files
- **Coverage**: All major Datalog features

## Fixture Categories

### Category 1: Basic Facts (0001-0050)
**50 fixtures** covering fundamental fact representation:
- Empty universe
- Single/multiple facts
- Different arities (unary through quinary)
- Value types (strings, integers, negative numbers)
- Mixed types
- Edge cases (empty strings, duplicates, large values)
- Relation patterns (symmetrical, chains, trees, DAGs, cycles)

### Category 2: Simple Queries (0051-0100)
**50 fixtures** testing query mechanisms:
- Wildcards in different positions
- Variable bindings (single, multiple, repeated)
- Constant matching
- String queries with special characters
- Negative numbers and large integers
- Empty results and duplicate handling
- Pattern matching variations

### Category 3: Simple Joins (0101-0150)
**50 fixtures** for join operations:
- Two-way joins (basic, multiple matches, no matches)
- Three-way joins
- Four-way joins
- Self-joins
- Cartesian products
- One-to-many and many-to-one joins
- Join filtering with constants

### Category 4: Simple Rules (0151-0200)
**50 fixtures** testing rule derivation:
- Single rule, single body clause
- Rules with multiple body clauses
- Multiple independent rules
- Chain rules (A→B, B→C, therefore A→C)
- Rules with constants
- Rules with wildcards
- Derived predicate testing

### Category 5: Recursion (0201-0250)
**50 fixtures** for recursive evaluation:
- Direct recursion
- Transitive closure variations
- Ancestor/descendant relationships
- Different graph shapes:
  - Linear chains
  - Trees
  - DAGs (Directed Acyclic Graphs)
  - Cyclic graphs
- Mutual recursion
- Deep recursion (20+ levels)
- Wide recursion (high branching factor)

### Category 6: Negation (0251-0300)
**50 fixtures** testing stratified negation:
- Simple negation
- Negation with joins
- Double negation
- Complement finding
- Non-existence checks
- Stratification correctness
- Interaction with positive rules

### Category 7: Built-in Predicates (0301-0350)
**50 fixtures** for built-in operations:
- Comparison operators (<, >, <=, >=)
- Equality (=, !=)
- Arithmetic (+, -, *, /)
- Range checks
- Type constraints
- Combined built-ins

### Category 8: Complex Queries (0351-0400)
**50 fixtures** combining multiple features:
- Multiple rules interacting
- Rules deriving from derived facts
- Long derivation chains
- Multiple query patterns
- All features combined

### Category 9: Graph Algorithms (0401-0450)
**50 fixtures** implementing graph algorithms:
- Reachability queries
- Path finding
- Connected components
- Cycle detection
- Transitive closure variants
- Graph property testing

### Category 10: Real-World Scenarios (0451-0502)
**52 fixtures** modeling practical applications:
- Family trees and genealogy
- Organizational hierarchies
- Social networks (follows, friends)
- File system paths
- Access control and permissions
- Data dependencies
- Comprehensive integration tests

## File Format

Each test consists of two files:

### Input File: `NNNN_name.datalog`
```datalog
% Test #NNNN: description

% Facts
person("alice").
edge(1, 2).

% Rules (if any)
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

### Expected Output: `NNNN_name.datalog.expected`
```json
{
  "facts": [
    {"predicate": "person", "args": ["alice"]},
    {"predicate": "edge", "args": [1, 2]}
  ],
  "query": "ancestor(X, Y)",
  "result": [
    {"X": "alice", "Y": "bob"}
  ]
}
```

## Complexity Progression

Fixtures are ordered by increasing complexity:

1. **0001-0100**: Foundational (facts and queries)
2. **0101-0200**: Basic operations (joins and rules)
3. **0201-0350**: Advanced features (recursion, negation, built-ins)
4. **0351-0502**: Real-world complexity (combined features, scenarios)

## Testing Strategy

### Unit Testing
Run individual fixtures to test specific features:
```bash
test_fixture 0010_transitive_closure
```

### Integration Testing
Run category ranges to test feature groups:
```bash
test_fixtures 0201-0250  # All recursion tests
```

### Regression Testing
Run all 500 fixtures to ensure no regressions:
```bash
test_all_fixtures
```

### Performance Testing
Use higher-numbered fixtures (0451-0502) for performance benchmarks:
- Fixture 0502: 100 facts stress test
- Fixture 0501: Comprehensive feature combination

## Validation

Each fixture has been:
- ✅ Systematically generated
- ✅ Paired with expected output
- ✅ Categorized by feature
- ✅ Ordered by complexity

## Usage in Tests

```ocaml
(* Example test runner *)
let test_fixture num =
  let datalog_file = Printf.sprintf "%04d_*.datalog" num in
  let expected_file = datalog_file ^ ".expected" in
  
  let program = Datalog.parse_file datalog_file in
  let expected = Json.from_file expected_file in
  
  let universe = Datalog.empty () in
  let universe = Datalog.load universe program in
  let result = Datalog.query universe ~query:expected.query in
  
  assert_equal expected.result result
```

## Maintenance

When adding new fixtures:
1. Use the next available number
2. Follow naming convention: `NNNN_descriptive_name`
3. Create both `.datalog` and `.datalog.expected`
4. Add to appropriate category
5. Update this documentation

## Coverage Summary

| Feature | Fixtures | Coverage |
|---------|----------|----------|
| Basic Facts | 50 | ✅ Complete |
| Queries | 50 | ✅ Complete |
| Joins | 50 | ✅ Complete |
| Rules | 50 | ✅ Complete |
| Recursion | 50 | ✅ Complete |
| Negation | 50 | ✅ Complete |
| Built-ins | 50 | ✅ Complete |
| Complex | 50 | ✅ Complete |
| Graphs | 50 | ✅ Complete |
| Real-world | 52 | ✅ Complete |
| **Total** | **502** | **✅ 100%** |

---

*Generated automatically for comprehensive Datalog testing*
