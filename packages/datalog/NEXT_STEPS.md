# Next Steps for Datalog Development

**Current Status**: Week 1, Day 2 Complete ✅  
**Next Phase**: Week 1, Day 3-5 - Universe & Public API

---

## Immediate Next Steps (Day 3-5)

### 1. Create Universe Module

**File**: `src/universe.ml` + `src/universe.mli`

```ocaml
open Std
open Collections

type t = {
  facts : (string, Value.t list Relation.t) HashMap.t;
  rules : Ast.rule Vector.t;
}

let create () = {
  facts = HashMap.create ();
  rules = Vector.create ();
}

let add_fact universe atom =
  (* atom must be ground (no variables) *)
  assert (Ast.is_ground atom);
  
  let pred = atom.predicate in
  let args = List.map (fun t ->
    match t with
    | Term.Const v -> v
    | _ -> failwith "add_fact: atom must be ground"
  ) atom.args in
  
  let rel = match HashMap.find universe.facts pred with
    | Some r -> r
    | None -> Relation.empty ()
  in
  
  let new_rel = Relation.merge rel (Relation.singleton args) in
  HashMap.insert universe.facts pred new_rel |> ignore;
  universe

let add_rule universe rule =
  Vector.push universe.rules rule;
  universe

let get_facts universe ~predicate =
  HashMap.find universe.facts predicate
  |> Option.unwrap_or ~default:(Relation.empty ())

let contains_fact universe atom =
  assert (Ast.is_ground atom);
  let pred = atom.predicate in
  let args = List.map (fun t ->
    match t with
    | Term.Const v -> v
    | _ -> failwith "contains_fact: atom must be ground"
  ) atom.args in
  
  match HashMap.find universe.facts pred with
  | Some rel -> Relation.contains rel args
  | None -> false
```

### 2. Update Public API

**Update**: `src/datalog.ml` + `src/datalog.mli`

Add Universe export:
```ocaml
module Universe = Universe
```

### 3. Create Unit Tests

**File**: `tests/test_universe.ml`

```ocaml
open Std
open Datalog

let test_universe_basics () =
  let u = Universe.create () in
  
  let edge12 = Ast.atom 
    ~predicate:"edge"
    ~args:[Term.Const (Value.Int 1); Term.Const (Value.Int 2)]
  in
  
  let u = Universe.add_fact u edge12 in
  
  assert (Universe.contains_fact u edge12);
  
  println "✓ Universe basics work"

let () =
  println "Testing Universe...";
  test_universe_basics ();
  println "✅ Universe tests passed!"
```

### 4. Integration Test

Verify end-to-end:
```bash
$ tusk build datalog
$ tusk build test_universe
$ ./target/debug/test_universe
```

---

## Week 2: Evaluation Engine

Once Universe is done, proceed to evaluation:

### Day 8-9: Variable (Semi-Naive)

**File**: `src/variable.ml` + `src/variable.mli`

Track recent vs stable facts for semi-naive evaluation.

### Day 10-11: Join (Performance Critical!)

**File**: `src/join.ml` + `src/join.mli`

Implement:
- Basic merge join
- Semi-naive delta join
- **Galloping search** (O(log n) speedup!)

### Day 12-13: Unification

**File**: `src/unify.ml` + `src/unify.mli`

Pattern matching and variable binding.

### Day 14: Iteration

**File**: `src/iteration.ml` + `src/iteration.mli`

Fixed-point loop coordination.

---

## Week 3: Rule Evaluation

### Day 15-17: Evaluator

**File**: `src/evaluator.ml` + `src/evaluator.mli`

The heart of the engine - runs rules to fixed point.

### Day 18-21: Query & Testing

- Query evaluation
- Wire everything together
- First end-to-end transitive closure!
- Start passing runtime test fixtures

---

## Week 4: Testing & Optimization

### Day 22-24: Runtime Tests

Make 200+ tests pass from `tests/runtime/fixtures/`

### Day 25-26: Performance

- Profile and optimize
- Benchmark against targets
- Add galloping search if not done

### Day 27-28: Poneglyph Integration

- Convert Poneglyph facts to Datalog
- Replace manual transitive with Datalog queries
- Benchmark performance gains

---

## Quick Reference Commands

```bash
# Build datalog
tusk build datalog

# Build and run tests
tusk build test_basics
./target/debug/test_basics

tusk build test_universe
./target/debug/test_universe

# Check what binaries/tests exist
tusk completions --binaries
tusk completions --tests

# Format code
tusk fmt
```

---

## Files to Create (Checklist)

Week 1:
- [x] value.ml + .mli
- [x] term.ml + .mli
- [x] ast.ml + .mli
- [x] relation.ml + .mli
- [ ] universe.ml + .mli ← NEXT!
- [ ] test_universe.ml

Week 2:
- [ ] variable.ml + .mli
- [ ] join.ml + .mli
- [ ] unify.ml + .mli
- [ ] iteration.ml + .mli

Week 3:
- [ ] evaluator.ml + .mli
- [ ] tests/runtime/runtime_tests.ml

Week 4:
- [ ] benchmarks/bench_datalog.ml
- [ ] Integration with poneglyph

---

## Resources

- **Reference implementations**: `./3rdparty/datafrog/`, `./3rdparty/crepe/`
- **Test fixtures**: `tests/runtime/fixtures/` (500 tests ready!)
- **Design docs**: `DESIGN.md`, `TESTING.md`
- **Plan**: `PLAN.md` (comprehensive 4-week roadmap)

---

## Remember

1. ✅ Always `open Std` at the top
2. ✅ Use `cell` not `ref` for mutable values  
3. ✅ Prefer abstract types in interfaces
4. ✅ Test compilation frequently
5. ✅ Document as you go

---

**Status**: Ready for Universe implementation! 🚀

Next: Create `src/universe.ml` + `.mli`
