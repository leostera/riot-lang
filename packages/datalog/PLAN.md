# DATALOG IMPLEMENTATION PLAN

**Goal**: Build a high-performance Datalog engine for querying Poneglyph graph databases, inspired by Datafrog, Crepe, and DataScript.

**Current Status**: Parser complete ✅ (150 tests pass), Runtime engine missing ❌ (0 tests pass)

**Timeline**: 4 weeks to production-ready v1.0

---

## Executive Summary

### What We Have
- ✅ **Parser infrastructure** (Ceibo-based, 150 parser tests)
- ✅ **650 test fixtures** ready (150 parser + 500 runtime)
- ✅ **Reference implementations** in `./3rdparty/` (Datafrog, Crepe, DataScript)
- ✅ **Design doc** (`DESIGN.md`) and **test strategy** (`TESTING.md`)

### What We're Building
- ❌ **Runtime evaluation engine** (semi-naive, joins, fixed-point iteration)
- ❌ **High-performance data structures** (sorted relations, galloping search)
- ❌ **Public API** for Poneglyph integration
- ❌ **Query optimization** (indexing, stratification)

### Performance Targets
- **10K facts**: < 10ms (instant)
- **100K facts**: < 1s (fast)
- **1M facts**: < 10s (reasonable)

### Key Performance Techniques (From Reference Implementations)
1. **Semi-naive evaluation** (Datafrog) - Only process Δ (delta) facts
2. **Galloping search** (Datafrog) - O(log n) instead of O(n) in sorted arrays
3. **Sorted relations** (Datafrog) - Enable fast merges and joins
4. **Stratification** (Crepe) - Topological ordering for correct negation
5. **SwissTable HashMap** (already in Std.Collections!) - SIMD lookups

---

## Architecture Overview

```
packages/datalog/
├── src/
│   ├── parser/                    # ✅ DONE (150 tests pass)
│   │   ├── lexer.ml
│   │   ├── parser.ml
│   │   ├── syntax_kind.ml
│   │   ├── token.ml
│   │   └── diagnostic.ml
│   │
│   ├── ast/                       # 🔨 Week 1 - Core types
│   │   ├── ast.ml                 # AST types from parser
│   │   ├── ast.mli
│   │   ├── term.ml                # Var | Const | Wildcard
│   │   ├── term.mli
│   │   ├── value.ml               # Int | String | Uri
│   │   └── value.mli
│   │
│   ├── runtime/                   # 🔨 Week 2-3 - Evaluation engine
│   │   ├── relation.ml            # Sorted tuple storage
│   │   ├── relation.mli
│   │   ├── variable.ml            # Semi-naive: recent + stable
│   │   ├── variable.mli
│   │   ├── iteration.ml           # Fixed-point loop
│   │   ├── iteration.mli
│   │   ├── join.ml                # Join algorithms + galloping
│   │   ├── join.mli
│   │   ├── unify.ml               # Pattern matching & unification
│   │   ├── unify.mli
│   │   ├── evaluator.ml           # Rule evaluation
│   │   └── evaluator.mli
│   │
│   ├── universe.ml                # 🔨 Week 1 - Main API
│   ├── universe.mli
│   ├── datalog.ml                 # 🔨 Week 1 - Public exports
│   └── datalog.mli
│
└── tests/
    ├── parser/                    # ✅ DONE
    │   └── parser_tests.ml
    ├── runtime/                   # 🔨 Week 2-4 - Make tests pass
    │   ├── fixtures/              # 500 tests ready
    │   └── runtime_tests.ml       # NEW: Test harness
    └── integration/               # 🔨 Week 4 - End-to-end
        └── poneglyph_tests.ml     # NEW: Poneglyph integration
```

---

## Phase 1: Core Data Structures (Week 1)

**Goal**: Build the foundation - types, storage, basic operations

### Implementation Files

#### `src/ast/value.ml` + `src/ast/value.mli`
Core value types for Datalog constants.

#### `src/ast/term.ml` + `src/ast/term.mli`
Terms in Datalog: variables, constants, wildcards.

#### `src/ast/ast.ml` + `src/ast/ast.mli`
AST representation of atoms, rules, queries.

#### `src/runtime/relation.ml` + `src/runtime/relation.mli`
Sorted tuple storage - the foundation of performance.

#### `src/universe.ml` + `src/universe.mli`
Fact and rule storage.

---

## Phase 2: Evaluation Engine (Week 2)

**Goal**: Implement semi-naive evaluation and joins

### Implementation Files

#### `src/runtime/variable.ml` + `src/runtime/variable.mli`
Track recent vs stable facts for semi-naive evaluation.

#### `src/runtime/join.ml` + `src/runtime/join.mli`
**CRITICAL PATH**: Join algorithms with galloping search.

#### `src/runtime/unify.ml` + `src/runtime/unify.mli`
Pattern matching and variable binding.

#### `src/runtime/iteration.ml` + `src/runtime/iteration.mli`
Fixed-point loop coordination.

---

## Phase 3: Rule Evaluation (Week 3)

**Goal**: Evaluate rules and queries

### Implementation Files

#### `src/runtime/evaluator.ml` + `src/runtime/evaluator.mli`
Rule execution and fixed-point computation.

---

## Phase 4: Testing & Optimization (Week 4)

**Goal**: Make tests pass, optimize, integrate with Poneglyph

### Files

#### `tests/runtime/runtime_tests.ml`
Test harness for 500 runtime fixtures.

#### `benchmarks/bench_datalog.ml`
Performance benchmarks.

#### Poneglyph Integration
Update `packages/poneglyph/src/graph_store.ml` with Datalog queries.

---

## Performance Targets

- **10K facts**: < 10ms (instant)
- **100K facts**: < 1s (fast)
- **1M facts**: < 10s (reasonable)

---

## Success Criteria

### Week 1
- ✅ AST types complete
- ✅ Relation working (sorted tuples)
- ✅ Universe stores facts
- ✅ Public API defined
- ✅ 40 unit tests passing

### Week 2
- ✅ Variable tracks recent/stable
- ✅ Join algorithms working (with galloping)
- ✅ Unification working
- ✅ Iteration fixed-point loop
- ✅ 70 unit tests passing

### Week 3
- ✅ Evaluator runs rules to fixed point
- ✅ Query evaluation working
- ✅ First end-to-end test passes
- ✅ 200 runtime tests passing

### Week 4
- ✅ 500 runtime tests passing
- ✅ Performance targets met
- ✅ Poneglyph integration complete
- ✅ Documentation complete
- ✅ Ready for production use

---

## Reference Implementations

Located in `./3rdparty/`:
- **Datafrog** (~2K lines) - Semi-naive evaluation, galloping search
- **Crepe** (~1.5K lines) - Stratification, compile-time optimization
- **DataScript** - Full-featured database

Key techniques learned:
1. Galloping search for O(log n) joins
2. Semi-naive evaluation (recent vs stable)
3. Sorted relations for fast merges
4. Stratification for negation

---

## Next Steps

1. Create AST types (`src/ast/`)
2. Build Relation data structure (`src/runtime/relation.ml`)
3. Implement Universe (`src/universe.ml`)
4. Wire up public API (`src/datalog.ml`)
5. Start Week 2: Join algorithms!

---

**Status**: Ready to execute! 🚀

For detailed specifications, see the full plan above.
