# Learning OCaml and Riot ML Development

## Lessons Learned

- Document critical code insights and development strategies in this file to avoid repeating mistakes and capture institutional knowledge
- Recognize that the lack of comprehensive testing in critical infrastructure is a significant risk that must be addressed systematically
- Develop a test-driven development (TDD) approach that builds test coverage incrementally, starting with the most critical components
- Build test suites that validate not just correctness, but also performance, concurrent behavior, and cross-platform compatibility

## OCaml Best Practices

- Avoid shadowing top-level bindings locally
- Prefer verbose names
- Prefer `next_state` or `new_state` over `state'` 
- Prefer MyModule.{ record... } over { MyModule.field = ... } 

## Tusk Build System Architecture

### Persistent Server Architecture
Tusk uses a persistent background server that manages all build operations, providing consistent state and intelligent caching across multiple client interfaces.

```
                    ┌→ ocamllsp ←→ tusk ocaml-merlin ←┐
                    │                                  │
Editor/AI → MCP → tusk mcp ←━━━━━━━━━━━━━━━━━━━━━━━━━┥ tusk server (persistent)
                    │                                  │
                    └→ tusk cli ←━━━━━━━━━━━━━━━━━━━━━┘
```

**Key Components:**
- **tusk server**: Persistent process managing build graph, compilation, and workspace state
- **tusk cli**: Human-friendly command interface (`tusk build`, `tusk run`, etc.)
- **tusk ocaml-merlin**: Bridge providing Merlin protocol for ocaml-lsp-server integration
- **tusk mcp**: Model Context Protocol server for AI agent integration (Claude, etc.)

**Benefits:**
- **Live Build Integration**: Auto-build on demand when LSP needs type information
- **Unified State**: Single source of truth for build configuration across all clients
- **Intelligent Caching**: Persistent server maintains build artifacts and dependency graph
- **AI-Friendly**: Direct structured access to build system for AI agents via MCP

**Implementation Notes:**
- The server auto-builds modules on demand when referenced by LSP
- No .merlin files needed - configuration served dynamically via ocaml-merlin protocol
- MCP interface provides richer operations than LSP (bulk refactors, project generation, etc.)

## Tusk Build System Development Roadmap

### Completed Features ✅
- `tusk build` - Build all packages in workspace
- `tusk build -p <package>` - Build specific package and dependencies with dependency graph filtering
- `tusk run` - Run binaries with auto-detection of multiple binaries
- `tusk run -b <binary>` - Run specific binary with auto-build if missing
- `tusk clean` - Clean build artifacts
- `tusk help` - Show help information
- Binary promotion to `target/debug/` for easy access
- Multi-worker parallel builds with proper dependency ordering
- Workspace scanning and package discovery
- Dependency graph construction and topological sorting

### Core Features Roadmap 🚧

**Week 1 (High Priority)**
1. **`tusk new [opts] path`** - Create new package at path
   - Initialize package structure with src/, dune files
   - Generate basic lib.ml/main.ml based on package type
   - Add to workspace configuration

2. **`tusk version`** - Show tusk version
   - Display version, commit hash, build date
   - Useful for debugging and support

3. **`tusk test`** - Run tests 
   - Discover and run test files/packages
   - Integration with existing test frameworks
   - Parallel test execution

**Week 2 (Package Management)**
4. **`tusk add <pkg[@vsn]>`** - Add dependency to workspace/package
   - Add to top-level workspace.toml by default
   - Use `-p <pkg>` flag to add to specific package
   - Version resolution and compatibility checking

5. **`tusk install -b <bin>`** - Build and install binary to ~/.tusk/bin
   - Global binary installation for tools
   - Similar to `cargo install`
   - PATH management integration

**Week 3 (Developer Experience)**
6. **`tusk fmt` / `tusk format`** - Code formatting
   - Integration with ocamlformat
   - Workspace-wide formatting with consistent configuration

7. **`tusk doc`** - Generate documentation
   - Integration with odoc
   - Cross-package documentation linking

8. **`tusk check`** - Type checking without building
   - Fast feedback for development
   - IDE integration support

**Week 4 (Advanced Features)**
9. **Incremental rebuilds** - Content-based hashing for smart rebuilds
   - Hash-based caching: `<hash> -> outputs`
   - Build node graph analysis for minimal rebuilds
   - Major performance improvement for large projects

### Advanced Features (Future)

**Automatic OCaml Improvements**
- **Folder-based namespacing**: `pkg/src/a/b/c.ml` → `Pkg__A__B__C.ml`
- **Auto CamelCase conversion**: `hello_world.ml` → `HelloWorld` module
- **Smart dependency inference**: Analyze imports to suggest missing dependencies

**Additional Commands**
- `tusk publish` - Publish packages to registry
- `tusk update` - Update dependencies to latest compatible versions  
- `tusk tree` - Show dependency tree visualization
- `tusk clean --all` - Clean all artifacts including dependencies
- `tusk bench` - Run benchmarks
- `tusk watch` - Watch files and auto-rebuild
- `tusk shell` - Open shell with package environment

### Implementation Notes
- Maintain cargo-like UX for familiar developer experience
- Focus on OCaml-specific intelligence and optimizations
- Keep the actor-based parallel build system for performance
- Preserve existing dependency graph filtering for `-p` builds
