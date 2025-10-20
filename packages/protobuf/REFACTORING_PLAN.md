# Protobuf Parser Refactoring Plan

## Problem Statement

The current parser implementation uses mutable cells (`Cell.create`) throughout, which goes against functional programming principles. While `MutCursor` is acceptable (it mutates position internally), we should avoid explicit mutable state in our code.

## Goals

1. **Minimize Mutable State**: Use `MutCursor` internally but avoid `Cell.create` 
2. **Functional Style**: Pass accumulated state through function parameters
3. **OCaml Test Runner**: Replace Python test runner with OCaml (like `http` package)

## Refactoring Strategy

### Pattern: Accumulator Parameters

**Before (with Cell):**
```ocaml
let parse_fields cursor =
  let fields = Cell.create [] in
  let rec loop () =
    match parse_field cursor with
    | Error e -> Error e
    | Ok field ->
        Cell.set fields (Cell.get fields @ [ field ]);
        loop ()
  in
  loop ()
```

**After (functional):**
```ocaml
let parse_fields cursor =
  let rec loop acc =
    match parse_field cursor with
    | Error _ -> Ok (List.rev acc)  (* Done accumulating *)
    | Ok field ->
        skip_whitespace cursor;
        (match peek cursor with
        | Some ',' ->
            advance cursor;
            loop (field :: acc)  (* Prepend for O(1) *)
        | _ -> Ok (List.rev (field :: acc)))
  in
  loop []
```

**Key Changes:**
- Replace `Cell.create []` with parameter `acc` 
- Use `::` (prepend) instead of `@` (append) for efficiency
- `List.rev` at the end to restore order
- Termination when we can't parse more (instead of explicit checks)

### Pattern: Multiple Accumulators

**Before:**
```ocaml
let parse_message cursor =
  let elements = Cell.create [] in
  let options = Cell.create [] in
  let rec loop () =
    match peek cursor with
    | Some 'o' ->
        match parse_option cursor with
        | Ok opt ->
            Cell.set options (Cell.get options @ [ opt ]);
            loop ()
    | _ ->
        match parse_field cursor with
        | Ok field ->
            Cell.set elements (Cell.get elements @ [ field ]);
            loop ()
  in
  loop ();
  { elements = Cell.get elements; options = Cell.get options }
```

**After:**
```ocaml
let parse_message cursor =
  let rec loop elements_acc options_acc =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some '}' ->
        advance cursor;
        Ok {
          elements = List.rev elements_acc;
          options = List.rev options_acc;
        }
    | Some 'o' ->
        (match parse_option cursor with
        | Error e -> Error e
        | Ok opt -> loop elements_acc (opt :: options_acc))
    | _ ->
        (match parse_field cursor with
        | Error e -> Error e
        | Ok field -> loop (field :: elements_acc) options_acc)
  in
  loop [] []
```

### Pattern: Optional Values

**Before:**
```ocaml
let parse_file cursor =
  let package = Cell.create None in
  (* ... *)
  match parse_package cursor with
  | Ok pkg -> Cell.set package (Some pkg)
  | Error _ -> ()
```

**After:**
```ocaml
type state = {
  package : string option;
  imports : string list;
  (* ... *)
}

let parse_file cursor =
  let rec loop state =
    match peek cursor with
    | Some 'p' ->
        (match parse_package cursor with
        | Ok pkg -> loop { state with package = Some pkg }
        | Error e -> Error e)
    | _ -> Ok state
  in
  loop { package = None; imports = []; }
```

## Test Runner Migration

### Why OCaml Instead of Python?

1. **Consistency**: Same language as implementation
2. **Type Safety**: Compile-time checks for test logic
3. **Integration**: Direct access to parser APIs
4. **Example**: The `http` package does this successfully

### Test Runner Structure

```ocaml
(* test/main.ml *)
open Std

let load_fixtures base_path suffix =
  (* Discover test files *)
  (* Return list of (name, test_file, expected_file) *)

let test_protofile_parse (name, proto_file, expected_file) =
  Test.case (format "Protofile: %s" name) (fun () ->
    let input = read_file proto_file in
    let expected = read_file expected_file in
    match Protobuf.ProtofileFormat.parse input with
    | Ok ast -> verify_ast ast expected
    | Error err -> Error (format "Parse error: %s" err))

let () =
  Miniriot.run
    ~main:(fun ~args ->
      let fixtures = load_fixtures "packages/protobuf/tests/fixtures" ".proto" in
      let tests = List.map test_protofile_parse fixtures in
      Test.Cli.main ~name:"protobuf" ~tests ~args ())
    ~args:Env.args
  |> exit
```

### Running Tests

```bash
# Build
tusk build -p protobuf

# Run tests
./target/debug/protobuf_test

# Run specific tests
./target/debug/protobuf_test --filter "simple"
```

## Implementation Plan

### Phase 1: Test Infrastructure ✅
- [x] Create `test/main.ml`
- [x] Add `[[bin]]` to `tusk.toml`
- [x] Implement fixture loading
- [x] Basic test cases for each format

### Phase 2: Refactor Parsers (In Progress)
- [ ] Refactor `parse_field_options` (simple case)
- [ ] Refactor `parse_ranges` 
- [ ] Refactor `parse_enum_value` list
- [ ] Refactor `parse_field` list in messages
- [ ] Refactor `parse_message` body
- [ ] Refactor `parse_service` body
- [ ] Refactor top-level `parse` function

### Phase 3: Test Implementation
- [ ] Implement AST comparison for protofile
- [ ] Implement field comparison for debug format
- [ ] Implement record comparison for wire format
- [ ] Add round-trip tests
- [ ] Add error case tests

### Phase 4: Cleanup
- [ ] Remove Python test runner
- [ ] Remove `protofileFormat_functional.ml` example
- [ ] Update documentation
- [ ] Verify all tests pass

## Benefits

### Code Quality
- **Easier to Reason About**: No hidden mutation
- **Easier to Test**: Pure functions easier to unit test
- **Better Performance**: Prepend (`::`) is O(1) vs append (`@`) is O(n)

### Maintainability
- **Standard Pattern**: Follow established OCaml conventions
- **Consistent with Riot**: Match style of `http` and other packages
- **Type Safety**: Compiler catches more errors

## Examples from Other Packages

### http Package Pattern
```ocaml
(* From http/src/main.ml *)
let load_fixtures base_path suffix =
  match Fs.read_dir (Path.v base_path) with
  | Error _ -> []
  | Ok iter ->
      let entries = Std.Iter.MutIterator.to_list iter in
      List.filter_map (fun path ->
        (* Filter and transform *)
      ) entries
```

This is the pattern we should follow:
1. Use `Fs` and `Path` from `Std`
2. Load fixtures from filesystem
3. Create test cases with `Test.case`
4. Run with `Test.Cli.main`

## Migration Checklist

Per-Parser Checklist:

### ProtofileFormat
- [ ] Remove all `Cell.create` calls
- [ ] Convert to accumulator parameters
- [ ] Update `parse` function signature if needed
- [ ] Test with fixtures

### DebugFormat
- [ ] Remove all `Cell.create` calls
- [ ] Convert to accumulator parameters
- [ ] Verify round-trip: parse → print → parse

### WireFormat
- [ ] Check for any mutable state (likely none)
- [ ] Verify encode/decode are pure
- [ ] Add round-trip tests

## Testing Strategy

### Unit Tests (per function)
```ocaml
let test_parse_ident () =
  let cursor = MutCursor.create "hello" in
  match Parser.parse_ident cursor with
  | Ok "hello" -> Ok ()
  | _ -> Error "parse_ident failed"
```

### Integration Tests (full files)
```ocaml
let test_simple_proto () =
  let input = {| syntax = "proto3"; message Test { string name = 1; } |} in
  match ProtofileFormat.parse input with
  | Ok ast ->
      (* Verify structure *)
      Ok ()
  | Error e -> Error e
```

### Round-Trip Tests
```ocaml
let test_roundtrip () =
  let original = (* ... *) in
  let encoded = WireFormat.encode original in
  match WireFormat.decode encoded with
  | Ok decoded ->
      if decoded = original then Ok ()
      else Error "Round-trip mismatch"
  | Error e -> Error e
```

## Next Steps

1. **Immediate**: Finish `test/main.ml` implementation
2. **Short-term**: Refactor one parser function to demonstrate pattern
3. **Medium-term**: Systematic refactoring of all parsers
4. **Long-term**: Comprehensive test coverage

---

**Note**: We can do this incrementally. The current implementation works; this refactoring improves code quality without changing functionality.
