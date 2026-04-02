# Actors

A minimal bootstrapping build system for building the real `riot` package.

## Purpose

Actors is a temporary build tool designed specifically to bootstrap the development of the full `riot` build system. It has just enough functionality to build OCaml packages following a simple convention-over-configuration approach.

## Features

- **Zero configuration**: No config files needed
- **Self-contained**: Downloads and builds its own OCaml toolchain
- **OCaml-focused**: Designed specifically for OCaml packages
- **Convention-based**: Discovers packages in `packages/` automatically
- **Simple CLI**: Just `actors build`
- **Bootstrap-ready**: Can build itself and other packages in the riot-ml project

## Project Layout

Actors expects this simple layout:

```
riot-ml/
├── packages/              # Package source code
│   ├── gluon/            # I/O library
│   ├── actors/         # Actor runtime
│   ├── actors/         # This bootstrap tool
│   └── riot/             # The real build system (to be built)
└── target/               # Build outputs (created automatically)
    └── debug/            # Debug build artifacts  
```

## Toolchain Management

Actors manages its own OCaml installation in `~/.riot/toolchains/`:

```
~/.riot/
└── toolchains/
    └── 5.3.0/           # OCaml version
        ├── bin/
        │   ├── ocamlc   # Byte-code compiler
        │   └── ocamlopt # Native compiler
        └── lib/         # Standard library
```

On first run, actors will:
1. Create `~/.riot/` directory
2. Download OCaml 5.3.0 source from GitHub
3. Configure, compile, and install OCaml
4. Use this toolchain for all builds

## Usage

```bash
# First run - downloads and builds OCaml toolchain (takes ~10 minutes)
actors build

# Subsequent runs - uses cached toolchain
actors build
```

## Build Rules

Actors follows simple conventions:

1. **Package Discovery**: Each directory in `packages/` is a package
2. **Library Building**: Non-`main.ml` `.ml` files are compiled into `package.cma`
3. **Executable Building**: `main.ml` files are compiled into `package_main`
4. **Output Location**: All artifacts go to `target/debug/`

## Example

```bash
cd riot-ml
actors build
```

**Output (first run):**
```
Configuration: Config(profile=debug, ocaml_version=5.3.0)
Building in /Users/user/riot-ml
Downloading and building OCaml 5.3.0...
Downloading: curl -L -o ~/.riot/toolchains/5.3.0/5.3.0.tar.gz https://github.com/ocaml/ocaml/archive/5.3.0.tar.gz
Extracting: cd ~/.riot/toolchains/5.3.0 && tar -xzf 5.3.0.tar.gz --strip-components=1
Configuring: cd ~/.riot/toolchains/5.3.0 && ./configure --prefix=~/.riot/toolchains/5.3.0
Building: cd ~/.riot/toolchains/5.3.0 && make -j4 world.opt
Installing: cd ~/.riot/toolchains/5.3.0 && make install
OCaml 5.3.0 installed successfully!
Discovered 3 packages
  - gluon
  - actors  
  - actors
Building package: gluon
[... build continues with installed toolchain ...]
```

**Output (subsequent runs):**
```
Configuration: Config(profile=debug, ocaml_version=5.3.0)
Building in /Users/user/riot-ml
OCaml 5.3.0 toolchain already available
Discovered 3 packages
  - gluon
  - actors  
  - actors
Building package: gluon
Building package: actors
Building package: actors
Build successful!
```

**Artifacts Created:**
- `target/debug/gluon.cma` (if gluon has non-main .ml files)
- `target/debug/gluon_main` (if gluon has main.ml)
- `target/debug/actors.cma`
- `target/debug/actors_main` 

## Limitations

Actors is intentionally minimal for bootstrapping:

- **No dependency resolution**: Builds packages in alphabetical order
- **No external dependencies**: Only builds what's in `packages/`
- **No caching**: Rebuilds everything each time
- **No configuration**: Hardcoded debug profile
- **Basic error handling**: Stops on first build failure

These limitations are acceptable since actors's only job is to build `riot`, which will be the real build system.

## Next Steps

Once `riot` is implemented, actors can be retired. The `riot` package will provide:
- Smart dependency resolution
- Content-addressable caching  
- Parallel builds using Actors
- TOML configuration
- Multiple build profiles
- Integration with external tools