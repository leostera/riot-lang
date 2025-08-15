# Tusk Package Management

## Overview

Tusk provides a built-in package management system with workspace-wide dependency resolution, a centralized package registry, and version constraints using the PubGrub algorithm.

## Core Principles

1. **Workspace-unified versions**: One version per dependency across the entire workspace
2. **Package isolation**: Dependencies are scoped to packages, not shared implicitly
3. **Registry-first**: All packages go through the Tusk package registry
4. **Source distribution**: Packages are distributed as source tarballs
5. **No OPAM compatibility**: Clean break from OPAM's complexity

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                      Workspace                          │
│  ┌────────────┐  ┌────────────┐  ┌────────────┐       │
│  │ Package A  │  │ Package B  │  │ Package C  │       │
│  │ deps: x@1  │  │ deps: x@1  │  │ deps: y@2  │       │
│  │       y@2  │  │       z@3  │  │            │       │
│  └────────────┘  └────────────┘  └────────────┘       │
│                          │                              │
│                    Unified Resolution                   │
│                    x@1.0.0, y@2.1.0, z@3.0.0           │
└─────────────────────────┬───────────────────────────────┘
                          │
                    HTTP Requests
                          │
              ┌───────────▼───────────┐
              │   Package Registry    │
              │  (HTTP API Server)    │
              │                       │
              │  /packages/list       │
              │  /packages/{name}     │
              │  /packages/publish   │
              └───────────────────────┘
```

## Dependency Specification

### In tusk.toml

```toml
[package]
name = "mylib"
version = "1.0.0"

[dependencies]
# Simple version
riot = "2.0.0"

# Version constraints (future)
gluon = ">=1.0.0, <2.0.0"
miniriot = "~1.5.0"  # >= 1.5.0, < 1.6.0

# Git dependencies (future)
experimental = { git = "https://github.com/user/repo", branch = "main" }
```

### Version Resolution Rules

1. **Exact**: `"1.0.0"` - exactly version 1.0.0
2. **Range** (future): `">=1.0.0, <2.0.0"` - any version in range
3. **Compatible** (future): `"~1.5.0"` - >= 1.5.0 but < 1.6.0
4. **Latest** (future): `"*"` - latest available version

## Command Line Interface

### Adding Dependencies

```bash
# Add to workspace root
$ tusk add riot
Added riot@2.1.0 to workspace

# Add specific version
$ tusk add riot@2.0.0
Added riot@2.0.0 to workspace

# Add to specific package
$ tusk add -p mypackage gluon
Added gluon@1.0.0 to mypackage

# From within a package directory
$ cd packages/mypackage
$ tusk add miniriot
Added miniriot@0.1.0 to mypackage
```

### Removing Dependencies

```bash
# Remove from workspace
$ tusk rm riot
Removed riot from workspace

# Remove from specific package
$ tusk rm -p mypackage gluon
Removed gluon from mypackage
```

### Publishing Packages

```bash
# Publish current package
$ tusk publish
Publishing mylib@1.0.0...
✓ Package published

# Publish specific package
$ tusk publish -p mypackage
Publishing mypackage@2.0.0...
✓ Package published
```

### Installing Dependencies

```bash
# Install all dependencies
$ tusk install
Resolving dependencies...
Downloading riot@2.1.0...
Downloading gluon@1.0.0...
✓ All dependencies installed

# Update dependencies (future)
$ tusk update
Checking for updates...
Updated riot from 2.0.0 to 2.1.0
```

## Implementation Details

### 1. Dependency Resolution (PubGrub)

The PubGrub algorithm provides sound, complete dependency resolution with clear error messages.

```ocaml
(* packages/tusk/src/resolver.ml *)

module Package = struct
  type t = {
    name : string;
    version : Version.t;
  }
end

module Constraint = struct
  type t = 
    | Exact of Version.t
    | Range of { min : Version.t option; max : Version.t option }
    | Compatible of Version.t  (* ~1.5.0 *)
end

module Resolution = struct
  type incompatibility = {
    package : string;
    constraint1 : Constraint.t;
    constraint2 : Constraint.t;
    source1 : string;  (* which package requires this *)
    source2 : string;
  }
  
  type result = 
    | Success of (string * Version.t) list
    | Conflict of incompatibility
end

let resolve_dependencies (workspace : Workspace.t) : Resolution.result =
  (* 1. Collect all dependencies from all packages *)
  let all_deps = collect_all_dependencies workspace in
  
  (* 2. Create PubGrub solver instance *)
  let solver = PubGrub.create () in
  
  (* 3. Add all constraints to solver *)
  List.iter (fun (pkg, constraint) ->
    PubGrub.add_constraint solver pkg constraint
  ) all_deps;
  
  (* 4. Solve *)
  match PubGrub.solve solver with
  | Ok solution -> Resolution.Success solution
  | Error conflict -> Resolution.Conflict (convert_conflict conflict)
```

### 2. Package Registry Service

HTTP API server for package discovery and distribution.

```ocaml
(* packages/package-registry/src/main.ml *)

module Registry = struct
  type package_metadata = {
    name : string;
    versions : Version.t list;
    latest : Version.t;
    description : string option;
    homepage : string option;
  }
  
  type package_version = {
    name : string;
    version : Version.t;
    dependencies : (string * Constraint.t) list;
    tarball_url : string;
    checksum : string;
    published_at : float;
  }
end

(* HTTP Endpoints *)
let routes = [
  (* List all packages *)
  GET "/api/v1/packages" -> list_packages;
  
  (* Get package metadata *)
  GET "/api/v1/packages/:name" -> get_package;
  
  (* Get specific version *)
  GET "/api/v1/packages/:name/:version" -> get_package_version;
  
  (* Download tarball *)
  GET "/api/v1/packages/:name/:version/tarball" -> download_tarball;
  
  (* Publish new package *)
  POST "/api/v1/packages/publish" -> publish_package;
  
  (* Search packages *)
  GET "/api/v1/search?q=:query" -> search_packages;
]
```

#### API Responses

```json
// GET /api/v1/packages
{
  "packages": [
    {
      "name": "riot",
      "latest": "2.1.0",
      "description": "Actor-model concurrency for OCaml"
    },
    {
      "name": "gluon",
      "latest": "1.0.0",
      "description": "High-performance I/O"
    }
  ]
}

// GET /api/v1/packages/riot
{
  "name": "riot",
  "versions": ["1.0.0", "2.0.0", "2.1.0"],
  "latest": "2.1.0",
  "description": "Actor-model concurrency for OCaml",
  "homepage": "https://github.com/riot-ml/riot"
}

// GET /api/v1/packages/riot/2.1.0
{
  "name": "riot",
  "version": "2.1.0",
  "dependencies": {
    "gluon": ">=1.0.0",
    "miniriot": "~0.1.0"
  },
  "tarball_url": "/api/v1/packages/riot/2.1.0/tarball",
  "checksum": "sha256:abc123...",
  "published_at": "2024-01-15T10:00:00Z"
}
```

### 3. Local Package Cache

Downloaded packages are cached locally to avoid re-downloading.

```
~/.tusk/
├── cache/
│   ├── riot-2.1.0/
│   │   ├── tusk.toml
│   │   └── src/
│   └── gluon-1.0.0/
│       ├── tusk.toml
│       └── src/
├── registry.json  # Cached registry metadata
└── checksums.json # Package checksums
```

### 4. Dependency Installation

```ocaml
(* packages/tusk/src/installer.ml *)

let install_dependencies workspace =
  (* 1. Resolve all dependencies *)
  match Resolver.resolve_dependencies workspace with
  | Error conflict -> 
      print_conflict_explanation conflict;
      Error "Dependency resolution failed"
  | Ok solution ->
      
  (* 2. Check cache for already installed *)
  let to_download = List.filter (fun (name, version) ->
    not (Cache.has_package name version)
  ) solution in
  
  (* 3. Download missing packages *)
  List.iter (fun (name, version) ->
    let metadata = Registry.get_package_version name version in
    let tarball = Registry.download_tarball metadata.tarball_url in
    
    (* Verify checksum *)
    if Checksum.verify tarball metadata.checksum then
      Cache.install_package name version tarball
    else
      failwith "Checksum mismatch!"
  ) to_download;
  
  (* 4. Update workspace lock file *)
  Workspace.write_lock_file workspace solution
```

### 5. Package Publishing

```ocaml
(* packages/tusk/src/publisher.ml *)

let publish_package package_path =
  (* 1. Load and validate tusk.toml *)
  let config = Workspace.load_package_config package_path in
  
  (* 2. Check version doesn't exist *)
  match Registry.get_package_version config.name config.version with
  | Some _ -> Error "Version already published"
  | None ->
  
  (* 3. Create source tarball *)
  let tarball = create_tarball package_path in
  let checksum = Checksum.calculate tarball in
  
  (* 4. Upload to registry *)
  Registry.publish {
    name = config.name;
    version = config.version;
    dependencies = config.dependencies;
    tarball = tarball;
    checksum = checksum;
  }
```

### 6. Lock File Format

`tusk.lock` ensures reproducible builds:

```toml
# This file is automatically generated by Tusk.
# It is not intended for manual editing.

[[package]]
name = "riot"
version = "2.1.0"
checksum = "sha256:abc123..."
dependencies = [
  "gluon@1.0.0",
  "miniriot@0.1.0"
]

[[package]]
name = "gluon"
version = "1.0.0"
checksum = "sha256:def456..."
dependencies = []

[[package]]
name = "miniriot"
version = "0.1.0"
checksum = "sha256:ghi789..."
dependencies = ["gluon@1.0.0"]
```

## Package Registry Implementation

### Storage Backend

For MVP, use filesystem storage:

```
registry-data/
├── packages/
│   ├── riot/
│   │   ├── metadata.json
│   │   └── versions/
│   │       ├── 2.0.0/
│   │       │   ├── manifest.json
│   │       │   └── tarball.tar.gz
│   │       └── 2.1.0/
│   │           ├── manifest.json
│   │           └── tarball.tar.gz
│   └── gluon/
│       ├── metadata.json
│       └── versions/
└── index.json  # Package list cache
```

### Authentication (Future)

```ocaml
type auth_token = {
  token : string;
  user : string;
  expires : float;
  scopes : string list;  (* ["publish:riot", "publish:gluon"] *)
}

(* Publishing requires authentication *)
POST /api/v1/packages/publish
Authorization: Bearer <token>
```

## Migration from OPAM

### Why Not OPAM Compatibility?

1. **Complexity**: OPAM's solver and metadata format are complex
2. **Build Systems**: OPAM packages use various build systems (dune, make, etc.)
3. **System Dependencies**: Many OPAM packages require system libraries
4. **Clean Break**: Opportunity to design something simpler

### Migration Strategy

For essential OPAM packages:

1. **Manual Porting**: Port popular packages to Tusk
2. **Automated Conversion** (future): Tool to convert simple OPAM packages
3. **Dual Publishing** (future): Allow packages to publish to both

## Error Messages

### Dependency Conflicts

```
Error: Dependency conflict detected

Package 'myapp' requires riot@2.0.0
Package 'mylib' requires riot@3.0.0

These constraints are incompatible. Consider:
- Updating myapp to support riot@3.0.0
- Downgrading mylib to use riot@2.0.0
- Making the constraints more flexible
```

### Missing Dependencies

```
Error: Package 'riot' not found in registry

Did you mean one of these?
- riot-core
- riot-testing

If this is a private package, ensure your registry URL is configured correctly.
```

### Version Not Found

```
Error: Version 2.5.0 of package 'riot' not found

Available versions:
- 2.1.0 (latest)
- 2.0.0
- 1.0.0

Try: tusk add riot@2.1.0
```

## Configuration

### Registry Configuration

```toml
# ~/.tusk/config.toml

[registry]
# Default public registry
url = "https://registry.tusk-lang.org"

# Alternative registries (future)
[[registry.alternates]]
name = "company"
url = "https://packages.mycompany.com"
priority = 1  # Check before public registry

# Authentication tokens
[auth]
"registry.tusk-lang.org" = "token_abc123..."
```

### Workspace Configuration

```toml
# workspace root tusk.toml

[workspace]
members = ["packages/*"]

# Workspace-wide dependencies
[workspace.dependencies]
riot = "2.1.0"
gluon = "1.0.0"

# Private registry (future)
[workspace.registry]
url = "https://packages.mycompany.com"
```

## Implementation Plan

### Phase 1: Core Package Management (MVP)
- [ ] PubGrub resolver implementation
- [ ] Local package cache
- [ ] `tusk add/rm` commands
- [ ] Simple exact version constraints
- [ ] Lock file generation

### Phase 2: Package Registry
- [ ] HTTP API server
- [ ] Package storage backend
- [ ] `tusk publish` command
- [ ] Package downloading
- [ ] Checksum verification

### Phase 3: Advanced Features
- [ ] Version ranges and constraints
- [ ] Authentication and authorization
- [ ] Package search
- [ ] Private registries
- [ ] Git dependencies

### Phase 4: Ecosystem
- [ ] Web UI for registry
- [ ] Package documentation hosting
- [ ] Download statistics
- [ ] Security advisories
- [ ] Automated testing of published packages

## Security Considerations

1. **Checksum Verification**: All packages verified against checksums
2. **HTTPS Only**: Registry communication over HTTPS
3. **Signed Packages** (future): GPG signatures for packages
4. **Dependency Scanning** (future): Check for known vulnerabilities
5. **Sandboxed Builds** (future): Build packages in isolated environments

## Performance Optimizations

1. **Parallel Downloads**: Download multiple packages concurrently
2. **Delta Updates** (future): Only download changed files
3. **CDN Distribution** (future): Serve packages from CDN
4. **Registry Caching**: Cache registry metadata locally
5. **Incremental Resolution**: Reuse previous resolution when possible

## Testing Strategy

### Unit Tests

```ocaml
[@test]
let test_version_parsing () =
  match Version.parse "1.2.3" with
  | Ok v when Version.to_string v = "1.2.3" -> Ok ()
  | _ -> Error "Version parsing failed"

[@test]
let test_constraint_matching () =
  let c = Constraint.parse ">=1.0.0, <2.0.0" in
  if Constraint.matches c (Version.of_string "1.5.0") then Ok ()
  else Error "Constraint should match 1.5.0"
```

### Integration Tests

```ocaml
[@test]
let test_package_installation () =
  (* Create test workspace *)
  let ws = Workspace.create_temp () in
  
  (* Add dependency *)
  Workspace.add_dependency ws "test-pkg" "1.0.0";
  
  (* Install *)
  match Installer.install_dependencies ws with
  | Ok () -> 
      if Cache.has_package "test-pkg" "1.0.0" then Ok ()
      else Error "Package not in cache"
  | Error e -> Error e
```

## FAQ

**Q: Why not just use OPAM?**
A: Tusk's integrated build system allows for simpler package management without OPAM's complexity.

**Q: Can I use private packages?**
A: Yes, you can run your own registry server for private packages.

**Q: How do I handle system dependencies?**
A: System dependencies should be documented in README. Future versions may support system dependency declarations.

**Q: What about Windows support?**
A: The package manager is platform-agnostic. Individual packages may have platform restrictions.

**Q: Can I vendor dependencies?**
A: Yes, you can commit the `.tusk/cache` directory for vendoring.

## Example Workflows

### Creating a New Library

```bash
$ tusk new mylib
$ cd mylib
$ tusk add riot
$ tusk add --dev tusk-test  # Dev dependency (future)
$ tusk build
$ tusk test
$ tusk publish
```

### Updating Dependencies

```bash
$ tusk update
Checking for updates...
  riot: 2.0.0 -> 2.1.0
  gluon: 1.0.0 (up to date)

Update riot to 2.1.0? [Y/n] y
✓ Updated riot to 2.1.0
```

### Debugging Resolution Issues

```bash
$ tusk resolve --verbose
Resolving dependencies...
  myapp requires riot@2.0.0
  mylib requires riot@2.1.0
  Attempting to unify...
  ✗ Conflict: incompatible versions

$ tusk resolve --explain
Dependency resolution failed because:
  1. myapp (from workspace) requires riot@2.0.0
  2. mylib (from workspace) requires riot@2.1.0
  3. These versions cannot be unified

Possible solutions:
  - Update myapp to use riot@2.1.0
  - Downgrade mylib to use riot@2.0.0
```