# Datalog Test Fixtures

This directory contains test fixtures for the Datalog implementation, organized from simple to complex.

## Fixture Format

Each test consists of two files:
- `NNNN_name.datalog` - The Datalog program (facts and rules)
- `NNNN_name.datalog.expected` - Expected output in JSON format

## Datalog Syntax

### Facts
```datalog
person("alice").
edge(1, 2).
parent("alice", "bob").
```

### Rules
```datalog
% Head :- Body
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

### Negation
```datalog
not_connected(X, Y) :- node(X), node(Y), !edge(X, Y).
```

### Variables
- Uppercase letters: `X`, `Y`, `Z`, `Name`, etc.
- Underscore `_` for anonymous variables

### Constants
- Strings: `"alice"`, `"bob"`
- Numbers: `1`, `2`, `42`
- Atoms: `engineer`, `doctor`

## Expected Output Format

```json
{
  "facts": [
    {"predicate": "edge", "args": [1, 2]}
  ],
  "rules": [
    {
      "head": {"predicate": "path", "args": ["X", "Y"]},
      "body": [{"predicate": "edge", "args": ["X", "Y"]}]
    }
  ],
  "query": "path(X, Y)",
  "result": [
    {"X": 1, "Y": 2}
  ]
}
```

## Test Progression

### Basic (0001-0005)
- Empty universe
- Simple facts
- Multiple facts
- Binary relations
- Simple joins

### Rules (0006-0014)
- Simple rule derivation
- Transitive closure
- Ancestor relationships
- Multiple rules

### Advanced (0015-0029)
- Stratified negation
- Same generation (complex recursion)
- Constants in rules
- Graph analysis

### Performance (0030+)
- Large datasets
- Complex rule interactions
- Stress tests

## Running Tests

```ocaml
(* Example test runner *)
let test_fixture filename =
  let program = read_datalog filename in
  let expected = read_json (filename ^ ".expected") in
  let universe = Datalog.empty () in
  let universe = Datalog.load universe program in
  let result = Datalog.query universe ~query:expected.query in
  assert_equal expected.result result
```
