# Tusk Build System Architecture

## Overview

Tusk uses a persistent background server that manages all build operations, providing consistent state and intelligent caching across multiple client interfaces.

## System Architecture

```
                    ┌→ ocamllsp ←→ tusk ocaml-merlin ←┐
                    │                                  │
Editor/AI → MCP → tusk mcp ←━━━━━━━━━━━━━━━━━━━━━━━━━┥ tusk server (persistent)
                    │                                  │
                    └→ tusk cli ←━━━━━━━━━━━━━━━━━━━━━┘
```

## Key Components

### tusk server
- Persistent process managing build graph, compilation, and workspace state
- Maintains in-memory build cache and dependency information
- Handles all build requests from various clients
- Auto-builds modules on demand when referenced by LSP

### tusk cli
- Human-friendly command interface (`tusk build`, `tusk run`, etc.)
- Lightweight client that communicates with the server
- Handles command-line parsing and user interaction
- Returns results and error messages to the terminal

### tusk ocaml-merlin
- Bridge providing Merlin protocol for ocaml-lsp-server integration
- Translates between tusk's build state and Merlin's type information needs
- No .merlin files needed - configuration served dynamically
- Enables IDE features like auto-completion and type inspection

### tusk mcp
- Model Context Protocol server for AI agent integration (Claude, etc.)
- Provides richer operations than LSP (bulk refactors, project generation, etc.)
- Direct structured access to build system for AI agents
- Enables AI-powered development workflows

## Core Benefits

### Live Build Integration
- Auto-build on demand when LSP needs type information
- No manual build steps required for IDE features
- Immediate feedback on code changes

### Unified State
- Single source of truth for build configuration across all clients
- Consistent view of project state between CLI, IDE, and AI tools
- No synchronization issues between different tools

### Intelligent Caching
- Persistent server maintains build artifacts and dependency graph
- Content-based hashing for smart incremental rebuilds
- Minimal rebuilds based on actual changes

### AI-Friendly
- Direct structured access to build system for AI agents via MCP
- Enables advanced automation and code generation
- Rich metadata about project structure and dependencies

## Internal Architecture

### Build Process Flow

1. **Workspace Scanning**
   - Discovers tusk.toml in workspace root
   - Finds all packages in workspace members
   - Builds initial package registry

2. **Dependency Analysis**
   - Uses ocamldep for accurate dependency information
   - Constructs directed acyclic graph (DAG) of dependencies
   - Performs topological sort for build ordering

3. **Parallel Build Execution**
   - Spawns multiple worker processes (actors)
   - Workers pull tasks from build queue
   - Each worker builds in isolated sandbox environment

4. **Artifact Management**
   - Sandboxed builds prevent contamination
   - Successful builds promote artifacts to target directory
   - Failed builds are isolated and reported

### Actor Model

Tusk uses Miniriot's actor model for concurrent builds:

- **Server Actor**: Coordinates build operations
- **Worker Actors**: Execute individual package builds
- **Message Passing**: All communication via typed messages
- **Fault Isolation**: Worker failures don't crash the system

### Build Messages

Key message types in the system:

- `ScanWorkspace`: Initialize workspace scanning
- `BuildPackage`: Request to build a specific package
- `TaskRequest`: Worker requesting next build task
- `TaskAssignment`: Server assigning package to worker
- `BuildComplete`: Worker reporting build success/failure
- `BuildFinished`: Overall build completion status

### Sandbox System

Each package builds in an isolated sandbox:

- Unique temporary directory per build
- Copies only required dependencies
- Prevents cross-package contamination
- Clean environment for reproducible builds

## Future Architecture Enhancements

### Content-Based Incremental Builds
- Hash all inputs (source files, dependencies, compiler flags)
- Cache outputs based on content hash
- Skip rebuilds when hash matches
- Dramatic performance improvements for large projects

### Distributed Builds
- Worker actors can run on remote machines
- Central server coordinates distributed compilation
- Share build cache across team/CI

### Package Registry Integration
- Direct integration with package registries
- Automatic dependency resolution
- Version constraint solving

### Hot Reloading
- File system watching for automatic rebuilds
- Push updates to running applications
- Development server integration