# Tusk Module Namespacing

## Overview

Tusk implements automatic module namespacing to prevent module name conflicts between packages. This is achieved using OCaml's `-no-alias-deps` compilation flag and module aliases, allowing multiple packages to have modules with the same name (like `Utils.ml` or `Common.ml`) without conflicts.

## How It Works

### 1. Module Name Transformation

When Tusk builds a package, it automatically prefixes all module names with the package name:

- Package: `my-package`
- Source file: `src/utils.ml`
- Compiled module: `My_package__Utils`

Note that hyphens in package names are converted to underscores to create valid OCaml module names.

### 2. Module Alias Generation

To maintain ergonomic imports within a package, Tusk generates an alias module that maps simple names to their namespaced versions:

**Generated `My_package__aliases.ml`:**
```ocaml
(* Auto-generated module aliases for package my-package *)
module Utils = My_package__Utils
module Common = My_package__Common
module Config = My_package__Config
```

### 3. Compilation Process

The build process follows these steps:

1. **Generate and compile the alias module** (`PackageName__aliases.ml`)
   - Compiled with `-no-alias-deps` flag
   - Produces both `.cmo` and `.cmi` files

2. **Compile all package modules** with namespaced names
   - Each module is compiled with `-open PackageName__aliases`
   - This allows modules within the package to reference each other using simple names

3. **Link everything** into a library (`.cma` file)
   - Includes the alias module and all namespaced modules

### 4. Module Re-exports

The main package module (e.g., `my_package.ml`) can re-export internal modules for public API:

```ocaml
(** my_package.ml - Main package interface *)

module Utils = Utils  (* Re-exports My_package__Utils *)
module Config = Config  (* Re-exports My_package__Config *)
(* Common is not re-exported, keeping it internal *)
```

## Example: The `std` Package

Let's look at how the `std` package works with namespacing:

### Source Structure
```
packages/std/src/
├── std.ml       # Main package module
├── std.mli      # Package interface
├── data.ml      # Data module
├── data.mli
├── log.ml       # Logging module
├── log.mli
└── ...
```

### Build Process

1. **Alias module generated** (`Std__aliases.ml`):
```ocaml
module Data = Std__Data
module Log = Std__Log
module Path = Std__Path
(* ... more aliases ... *)
```

2. **Modules compiled with namespaced names**:
   - `data.ml` → `Std__Data.cmo`
   - `log.ml` → `Std__Log.cmo`
   - `path.ml` → `Std__Path.cmo`

3. **Main module re-exports** (`std.ml`):
```ocaml
module Data = Data
module Log = Log
module Path = Path
(* ... more exports ... *)
```

### Using the Package

From other packages, you can use the `std` modules in several ways:

```ocaml
(* Option 1: Direct qualified access *)
let json = Std.Data.Json.parse str

(* Option 2: Open the package *)
open Std
let json = Data.Json.parse str

(* Option 3: Open a specific submodule *)
open Std.Data
let json = Json.parse str  (* This works because Data re-exports Json *)
```

## Benefits

1. **No Name Conflicts**: Multiple packages can have `utils.ml`, `common.ml`, etc.
2. **Clean APIs**: Package interfaces remain clean with simple module names
3. **Incremental Compilation**: `-no-alias-deps` enables better separate compilation
4. **Backward Compatible**: Packages can still be used with familiar module names

## Implementation Details

### The `-no-alias-deps` Flag

This OCaml compiler flag tells the compiler not to include dependencies when compiling the alias module. This is crucial for:
- Avoiding circular dependencies
- Enabling true separate compilation
- Reducing rebuild cascades when modules change

### Critical Files

When using module aliases, both `.cmo` and `.cmi` files must be available:
- `.cmo`: Contains the compiled bytecode
- `.cmi`: Contains the compiled interface (type information)

Tusk ensures both files are included in build outputs and copied to dependent packages.

### Package Name Safety

Package names with hyphens are automatically converted:
- Package name: `my-awesome-lib`
- Module prefix: `My_awesome_lib__`
- Alias module: `My_awesome_lib__aliases`

## Troubleshooting

### "Module not found" Errors

If you get errors like "no cmi file was found in path for module X":
- Ensure the package is listed as a dependency
- Check that both `.cmo` and `.cmi` files are in the build output
- Verify the alias module was generated and compiled first

### "This is an alias for module X, which is missing"

This typically means:
- The alias module's `.cmi` file is not available
- The module wasn't compiled with the correct `-open` flag
- There's a missing dependency in the build order

## Best Practices

1. **Keep package interfaces clean**: Only re-export modules that are part of your public API
2. **Use consistent naming**: Follow OCaml conventions for module names
3. **Document exports**: Make it clear which modules are public vs internal
4. **Avoid deep nesting**: Prefer flat module structures for better usability