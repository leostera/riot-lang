# Tusk Test Framework

## Overview

`tusk test` provides a lightweight, built-in testing framework that discovers and runs tests across your entire workspace. Tests are regular OCaml functions marked with the `[@test]` attribute.

## Motivation

### Why Built-in Testing?

1. **Zero configuration**: No external test frameworks to configure
2. **Build system integration**: Tusk already understands your project structure
3. **Process isolation**: Each test module runs in its own process
4. **Simple mental model**: Tests are just functions that return results

### Design Principles

- **Simplicity**: Tests are functions with a specific signature
- **Discoverability**: `[@test]` attributes make tests easy to find
- **Isolation**: Test modules run as separate executables
- **Minimal magic**: Generated code is straightforward and debuggable

## Test Format

### Basic Test Structure

```ocaml
(* any_module.ml *)

(* Regular code *)
let add x y = x + y

(* Test function *)
[@test]
let test_addition () =
  if add 2 2 = 4 then
    Ok ()
  else
    Error "2 + 2 should equal 4"

[@test]
let test_addition_commutative () =
  if add 2 3 = add 3 2 then
    Ok ()
  else
    Error "addition should be commutative"
```

### Test Signature

All test functions must have the signature:
```ocaml
unit -> (unit, string) result
```

- `Ok ()` indicates the test passed
- `Error msg` indicates the test failed with a reason
- Uncaught exceptions are treated as test errors

## Implementation Details

### 1. Test Discovery Phase

When `tusk test` is invoked:

```
workspace/
├── packages/
│   ├── foo/
│   │   ├── src/
│   │   │   ├── lib.ml      [@test] let test_foo = ...
│   │   │   └── utils.ml    [@test] let test_bar = ...
│   └── bar/
│       └── src/
│           └── main.ml     [@test] let test_baz = ...
```

**Discovery Algorithm:**
1. Scan all packages in dependency order
2. For each package, scan all `.ml` files
3. Parse each file looking for `[@test]` attributes
4. Extract test function names

**Implementation Notes:**
- Use simple regex matching: `\[@test\]\s+let\s+(\w+)`
- Store mapping: `package -> file -> [test_names]`
- Skip generated `*_test.ml` files to avoid recursion

### 2. Test Runner Generation

For each file with tests, generate a corresponding `*_test.ml` file:

**Input:** `foo.ml` with tests `test_a` and `test_b`

**Generated:** `foo_test.ml`
```ocaml
(* GENERATED FILE - DO NOT EDIT *)
(* Original source: foo.ml *)

(* Include all original code *)
[@@@ocaml.warning "-32-34-37"] (* Disable unused warnings *)
include struct
  (* Original foo.ml content here *)
  let add x y = x + y
  
  [@test]
  let test_a () = Ok ()
  
  [@test] 
  let test_b () = Ok ()
end

(* Test runner *)
let () =
  let module_name = "Foo" in
  let tests = [
    ("test_a", test_a);
    ("test_b", test_b);
  ] in
  Tusk_test.run_module ~name:module_name ~tests
```

**Key Decisions:**
- Include original code via `include struct ... end` to avoid module dependencies
- Disable warnings for unused code in test context
- Pass module name for better reporting

### 3. Test Compilation

Each `*_test.ml` file is compiled as an executable:

```bash
# Build test executable
ocamlc -I package/src -I target/out/package \
  package_deps.cma \
  tusk_test.cmo \
  foo_test.ml \
  -o target/test/package/foo_test
```

**Build Strategy:**
1. Place test files in `target/test/<package>/`
2. Link against:
   - Package dependencies (already built)
   - Tusk test runner module (minimal, embedded in tusk)
   - The generated test file
3. Output executables to `target/test/<package>/`

### 4. Test Execution

Run all test executables and collect results:

```
$ tusk test
Running tests for workspace...

Package: foo
  foo.ml
    ✓ test_a
    ✓ test_b
  utils.ml
    ✓ test_helper
    ✗ test_broken: Expected 5 but got 4

Package: bar
  main.ml
    💥 test_crash: Division_by_zero

Summary: 4 passed, 1 failed, 1 error
```

**Execution Flow:**
1. Run tests in package dependency order
2. For each package, run test executables sequentially
3. Capture exit codes and output
4. Aggregate results across all packages

### 5. Test Runner Module

Embedded in tusk as `tusk_test.ml`:

```ocaml
(* packages/tusk/src/tusk_test.ml *)

type test = string * (unit -> (unit, string) result)

type test_result = {
  name : string;
  result : [ `Pass | `Fail of string | `Error of exn ];
  duration : float;
}

let run_module ~name ~tests =
  Printf.printf "  %s\n" name;
  
  let results = List.map (fun (test_name, test_fn) ->
    let start = Unix.gettimeofday () in
    let result = 
      try
        match test_fn () with
        | Ok () -> `Pass
        | Error msg -> `Fail msg
      with exn -> `Error exn
    in
    let duration = Unix.gettimeofday () -. start in
    
    (* Print immediate feedback *)
    (match result with
    | `Pass -> Printf.printf "    ✓ %s\n" test_name
    | `Fail msg -> Printf.printf "    ✗ %s: %s\n" test_name msg
    | `Error exn -> Printf.printf "    💥 %s: %s\n" test_name (Printexc.to_string exn));
    
    { name = test_name; result; duration }
  ) tests in
  
  (* Exit with appropriate code *)
  let has_failures = List.exists (fun r -> 
    match r.result with `Pass -> false | _ -> true
  ) results in
  
  if has_failures then exit 1 else exit 0
```

## Command Line Interface

### Basic Usage

```bash
# Run all tests
$ tusk test

# Run tests for specific package
$ tusk test -p mypackage

# Run tests matching pattern (future)
$ tusk test --filter "test_addition*"

# Verbose output with timings (future)
$ tusk test -v
```

### Exit Codes

- `0`: All tests passed
- `1`: At least one test failed or errored
- `2`: Build/compilation error

## Future Enhancements

### Phase 1 (Current)
- [x] Basic test discovery via `[@test]` attribute
- [x] Test execution with pass/fail reporting
- [x] Process isolation per test module

### Phase 2 (Next)
- [ ] Parallel test execution
- [ ] Test filtering by name pattern
- [ ] Timing information
- [ ] Better error reporting with source locations

### Phase 3 (Future)
- [ ] Module-based test grouping
- [ ] Setup/teardown hooks
- [ ] Property-based testing support
- [ ] Coverage reporting
- [ ] Watch mode for continuous testing

## Implementation Checklist

1. **Test Discovery** (`test_discovery.ml`)
   - [ ] Scan workspace for .ml files
   - [ ] Parse files for [@test] attributes
   - [ ] Extract test function names
   - [ ] Build package -> file -> tests mapping

2. **Test Generation** (`test_generator.ml`)
   - [ ] Generate *_test.ml files
   - [ ] Include original source
   - [ ] Add test runner invocation
   - [ ] Handle module paths correctly

3. **Test Building** (`test_builder.ml`)
   - [ ] Compile test files to executables
   - [ ] Link with dependencies
   - [ ] Place in target/test/

4. **Test Execution** (`test_executor.ml`)
   - [ ] Run test executables
   - [ ] Capture output and exit codes
   - [ ] Report results
   - [ ] Return appropriate exit code

5. **CLI Integration** (`cli.ml`)
   - [ ] Add `test` subcommand
   - [ ] Parse test-specific options
   - [ ] Invoke test pipeline

## Example Test Scenarios

### Scenario 1: Simple Unit Tests

```ocaml
(* math.ml *)
let add x y = x + y
let multiply x y = x * y

[@test]
let test_addition () =
  if add 2 3 = 5 then Ok ()
  else Error "Addition failed"

[@test]
let test_multiplication () =
  if multiply 3 4 = 12 then Ok ()
  else Error "Multiplication failed"
```

### Scenario 2: Testing with Side Effects

```ocaml
(* file_ops.ml *)
[@test]
let test_file_operations () =
  let path = "/tmp/test_file.txt" in
  match File.write ~path ~content:"test" with
  | Error e -> Error (Printf.sprintf "Write failed: %s" (show_error e))
  | Ok () ->
      match File.read ~path with
      | Error e -> Error (Printf.sprintf "Read failed: %s" (show_error e))
      | Ok content ->
          if content = "test" then Ok ()
          else Error "Content mismatch"
```

### Scenario 3: Testing Concurrent Code

```ocaml
(* concurrent.ml *)
[@test]
let test_spawn_and_receive () =
  Miniriot.run ~main:(fun () ->
    let child = Miniriot.spawn (fun () ->
      Miniriot.send (Miniriot.self ()) (Message.Text "hello");
      Normal
    ) in
    
    match Miniriot.receive_any () with
    | Message.Text "hello" -> Normal
    | _ -> Exception (Failure "Unexpected message")
  ) |> function
  | 0 -> Ok ()
  | _ -> Error "Process failed"
```

## Debugging Generated Tests

Generated test files are kept in `target/test/` for inspection:

```bash
# View generated test file
$ cat target/test/mypackage/foo_test.ml

# Run individual test executable
$ ./target/test/mypackage/foo_test

# Debug with ocamldebug
$ ocamldebug ./target/test/mypackage/foo_test
```

## Best Practices

1. **Test Naming**: Use descriptive names prefixed with `test_`
2. **Error Messages**: Provide clear failure reasons in `Error` returns
3. **Isolation**: Each test should be independent
4. **Fast Tests**: Keep individual tests fast (< 1 second)
5. **Deterministic**: Avoid time-dependent or random behavior

## FAQ

**Q: Why not use OUnit/Alcotest/etc?**
A: Tusk test is designed to be zero-dependency and fully integrated with the build system. You can still use external frameworks if needed.

**Q: Can I test internal/private functions?**
A: Yes! Tests in the same file have access to all definitions in that file.

**Q: How do I test across module boundaries?**
A: Tests can reference any module available to the package. The generated test includes the package's dependencies.

**Q: What about async/concurrent tests?**
A: Tests run synchronously, but you can test concurrent code by running a Miniriot scheduler within the test.

**Q: Can I skip tests?**
A: Currently no, but you can comment out the `[@test]` attribute or rename the function.