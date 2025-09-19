# Test Projects for tusk-depgraph

These test projects verify that our dependency graph builder correctly handles various OCaml project structures.

## Test Projects

### 1. `simple/` - Basic Dependencies
- Tests simple module dependencies
- `utils.ml` <- `math.ml` <- `main.ml`
- Expected: Correct topological ordering

### 2. `interfaces/` - Interface Files
- Tests .mli and .ml file relationships
- `logger.mli` + `logger.ml` <- `app.ml`
- Expected: .mli files compiled before .ml files

### 3. `subdirs/` - Subdirectories with Namespacing
- Tests nested modules with library interfaces
- `core/` directory with `types.ml`, `config.ml`, and `core.ml` (library interface)
- `ui/` directory with `display.ml` and `ui.mli`
- Expected: Generated alias files, proper namespacing (e.g., Core.Types, Ui.Display)

### 4. `circular/` - Circular Dependencies (Error Case)
- Tests circular dependency detection
- `a.ml` depends on `b.ml` which depends on `a.ml`
- Expected: Some nodes missing from topological sort

### 5. `external_deps/` - External Dependencies
- Tests tracking of stdlib/Unix dependencies
- Uses Unix, String, List modules
- Expected: External dependencies shown in analysis

## Running Tests

Once the kernel build is complete with proper C stub linking:
```bash
./run_tests.sh
```

This will:
1. Build tusk-depgraph with kernel and std libraries
2. Run the tool on each test project
3. Verify the expected output (topological sort, dependencies, warnings)