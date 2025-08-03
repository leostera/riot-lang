# Minitusk

A minimal bootstrapping build system for building the real `tusk` package.

## Purpose

Minitusk is a temporary build tool designed specifically to bootstrap the development of the full `tusk` build system. It has just enough functionality to build OCaml packages following a simple convention-over-configuration approach.

## Features

- **Zero configuration**: No config files needed
- **Self-contained**: Downloads and builds its own OCaml toolchain
- **OCaml-focused**: Designed specifically for OCaml packages
- **Convention-based**: Discovers packages in `packages/` automatically
- **Simple CLI**: Just `minitusk build`
- **Bootstrap-ready**: Can build itself and other packages in the riot-ml project

## Project Layout

Minitusk expects this simple layout:

```
riot-ml/
├── packages/              # Package source code
│   ├── gluon/            # I/O library
│   ├── miniriot/         # Actor runtime
│   ├── minitusk/         # This bootstrap tool
│   └── tusk/             # The real build system (to be built)
└── target/               # Build outputs (created automatically)
    └── debug/            # Debug build artifacts  
```

## Toolchain Management

Minitusk manages its own OCaml installation in `~/.tusk/toolchains/`:

```
~/.tusk/
└── toolchains/
    └── 5.3.0/           # OCaml version
        ├── bin/
        │   ├── ocamlc   # Byte-code compiler
        │   └── ocamlopt # Native compiler
        └── lib/         # Standard library
```

On first run, minitusk will:
1. Create `~/.tusk/` directory
2. Download OCaml 5.3.0 source from GitHub
3. Configure, compile, and install OCaml
4. Use this toolchain for all builds

## Usage

```bash
# First run - downloads and builds OCaml toolchain (takes ~10 minutes)
minitusk build

# Subsequent runs - uses cached toolchain
minitusk build
```

## Build Rules

Minitusk follows simple conventions:

1. **Package Discovery**: Each directory in `packages/` is a package
2. **Library Building**: Non-`main.ml` `.ml` files are compiled into `package.cma`
3. **Executable Building**: `main.ml` files are compiled into `package_main`
4. **Output Location**: All artifacts go to `target/debug/`

## Example

```bash
cd riot-ml
minitusk build
```

**Output (first run):**
```
Configuration: Config(profile=debug, ocaml_version=5.3.0)
Building in /Users/user/riot-ml
Downloading and building OCaml 5.3.0...
Downloading: curl -L -o ~/.tusk/toolchains/5.3.0/5.3.0.tar.gz https://github.com/ocaml/ocaml/archive/5.3.0.tar.gz
Extracting: cd ~/.tusk/toolchains/5.3.0 && tar -xzf 5.3.0.tar.gz --strip-components=1
Configuring: cd ~/.tusk/toolchains/5.3.0 && ./configure --prefix=~/.tusk/toolchains/5.3.0
Building: cd ~/.tusk/toolchains/5.3.0 && make -j4 world.opt
Installing: cd ~/.tusk/toolchains/5.3.0 && make install
OCaml 5.3.0 installed successfully!
Discovered 3 packages
  - gluon
  - miniriot  
  - minitusk
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
  - miniriot  
  - minitusk
Building package: gluon
Building package: miniriot
Building package: minitusk
Build successful!
```

**Artifacts Created:**
- `target/debug/gluon.cma` (if gluon has non-main .ml files)
- `target/debug/gluon_main` (if gluon has main.ml)
- `target/debug/miniriot.cma`
- `target/debug/minitusk_main` 

## Limitations

Minitusk is intentionally minimal for bootstrapping:

- **No dependency resolution**: Builds packages in alphabetical order
- **No external dependencies**: Only builds what's in `packages/`
- **No caching**: Rebuilds everything each time
- **No configuration**: Hardcoded debug profile
- **Basic error handling**: Stops on first build failure

These limitations are acceptable since minitusk's only job is to build `tusk`, which will be the real build system.

## Next Steps

Once `tusk` is implemented, minitusk can be retired. The `tusk` package will provide:
- Smart dependency resolution
- Content-addressable caching  
- Parallel builds using Miniriot
- TOML configuration
- Multiple build profiles
- Integration with external tools