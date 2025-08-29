# MCP Tools Roadmap for Tusk Build System

## Core Build & Development Tools

### 1. Type & Error Analysis
- **`typecheck`** - Fast incremental type checking without building
  - Check specific files or packages for type errors
  - Much faster than full build for iterative development
  
- **`explain_error`** - Deep dive into compilation errors
  - Provide detailed explanations of type mismatches
  - Suggest fixes based on common patterns
  - Show type inference chain that led to error

- **`infer_type`** - Get type at any position in code
  - Show inferred type of expression at cursor
  - Display module signatures
  - Show variant/record field types

### 2. Code Generation & Scaffolding
- **`generate_interface`** - Auto-generate .mli from .ml
  - Infer minimal public interface
  - Option to expose all or selective exports
  - Preserve documentation comments

- **`scaffold_module`** - Create new module with boilerplate
  - Generate matching .ml/.mli pair
  - Add common patterns (functors, module types)
  - Auto-add to build configuration

- **`derive`** - Generate boilerplate for types
  - Equality, comparison, show, serialization
  - Custom derivers for project patterns
  - Update when type changes

### 3. Refactoring Tools
- **`rename_symbol`** - Rename across entire codebase
  - Handle modules, types, values, fields
  - Update all references including .mli files
  - Preserve formatting and comments

- **`extract_function`** - Extract code into new function
  - Infer parameters and return type
  - Handle closures and free variables
  - Update call sites

- **`inline_function`** - Inline function at call sites
  - Handle single or all occurrences
  - Preserve semantics with let-bindings

- **`change_signature`** - Modify function signatures
  - Add/remove/reorder parameters
  - Update all call sites
  - Handle optional/labeled arguments

### 4. Navigation & Search
- **`find_definition`** - Jump to definition
  - Handle local and external modules
  - Support ppx-generated code
  - Navigate through functors

- **`find_references`** - Find all usages
  - Scope to file/package/workspace
  - Include type occurrences
  - Show context around usage

- **`find_implementations`** - Find module implementations
  - For module types and signatures
  - Show all modules matching interface
  - Navigate functor applications

### 5. Testing & Quality
- **`run_tests`** - Smart test execution
  - Run tests for changed modules only
  - Support watch mode
  - Filter by test name patterns

- **`coverage_report`** - Code coverage analysis
  - Show uncovered lines
  - Package-level summaries
  - Suggest test cases for uncovered paths

- **`suggest_tests`** - Generate test cases
  - Based on function signatures
  - Property-based test suggestions
  - Edge case identification

### 6. Dependency Management
- **`add_dependency`** - Add package dependencies
  - Resolve versions automatically
  - Check compatibility
  - Update lock files

- **`dependency_graph`** - Visualize dependencies
  - Show direct and transitive deps
  - Identify circular dependencies
  - Find unused dependencies

- **`upgrade_dependencies`** - Smart dependency updates
  - Show breaking changes
  - Run tests after upgrade
  - Rollback on failure

### 7. Performance & Optimization
- **`profile_build`** - Build performance analysis
  - Identify slow modules
  - Show parallelization opportunities
  - Cache hit rates

- **`optimize_imports`** - Clean up module opens/includes
  - Remove unused opens
  - Convert to explicit imports
  - Sort and group imports

- **`dead_code_analysis`** - Find unused code
  - Unused functions, types, modules
  - Confidence scores
  - Safe removal suggestions

### 8. Documentation & Learning
- **`generate_docs`** - Create documentation
  - Extract from comments
  - Generate examples from tests
  - Cross-reference types

- **`explain_concept`** - OCaml concept explanations
  - Explain language features in context
  - Show idiomatic usage patterns
  - Link to learning resources

- **`suggest_idioms`** - Make code more idiomatic
  - Replace imperative with functional patterns
  - Use standard library effectively
  - Apply community conventions

### 9. Project Management
- **`create_project`** - Initialize new project
  - Choose from templates (CLI, web, library)
  - Set up testing, CI, documentation
  - Configure build system

- **`module_stats`** - Code metrics and analysis
  - Complexity metrics
  - Line counts by type (code/test/docs)
  - Technical debt indicators

- **`migrate_syntax`** - Update to newer OCaml versions
  - Apply syntax improvements
  - Use new standard library features
  - Fix deprecations

### 10. Integration Tools
- **`format_code`** - Apply OCamlformat
  - Format changed files only
  - Check formatting in CI mode
  - Auto-fix common issues

- **`lint`** - Run linting checks
  - Code style violations
  - Common bugs and anti-patterns
  - Security issues

- **`git_hooks`** - Manage git hooks
  - Pre-commit formatting/linting
  - Commit message validation
  - Pre-push test running

## Advanced Specialized Tools

### 11. Concurrency & Riot-specific
- **`trace_messages`** - Trace actor messages
  - Show message flow between processes
  - Identify deadlocks
  - Performance bottlenecks

- **`visualize_supervision`** - Show supervision trees
  - Live process hierarchies
  - Restart strategies
  - Resource usage

### 12. Incremental Development
- **`hot_reload`** - Live code reloading
  - Reload changed modules
  - Preserve application state
  - Rollback on errors

- **`repl_context`** - REPL with project context
  - Load project modules
  - Access to dependencies
  - Save/restore sessions

### 13. Cross-compilation & Deployment
- **`cross_compile`** - Build for different targets
  - Native, bytecode, JavaScript
  - Different architectures
  - Static binaries

- **`package_release`** - Create release artifacts
  - Binary distributions
  - Source tarballs
  - Docker images

## Implementation Priority

### Phase 1 (Immediate - Week 1)
1. `typecheck` - Critical for fast feedback
2. `find_definition` - Essential navigation
3. `find_references` - Essential navigation
4. `rename_symbol` - Basic refactoring
5. `run_tests` - Testing workflow

### Phase 2 (Week 2-3)
6. `generate_interface` - Common task
7. `explain_error` - Better error understanding
8. `add_dependency` - Package management
9. `format_code` - Code consistency
10. `scaffold_module` - Productivity boost

### Phase 3 (Week 4+)
11. `derive` - Reduce boilerplate
12. `optimize_imports` - Code cleanup
13. `coverage_report` - Quality metrics
14. `profile_build` - Performance
15. `dead_code_analysis` - Maintenance

### Future Phases
- Remaining tools based on user feedback and needs
- Integration with AI assistants for smarter suggestions
- Custom tools for specific project patterns

## Technical Notes

### MCP Protocol Extensions Needed
- Streaming responses for long operations
- Progress indicators for builds/tests
- Cancellation support
- File watching integration
- Multi-file transactions

### Integration Points
- LSP server for editor integration
- CLI for terminal usage
- Web UI for visualizations
- CI/CD pipeline integration
- AI assistant integration (Claude, Copilot)

### Performance Considerations
- Incremental analysis using build cache
- Parallel processing where possible
- Lazy loading of analysis data
- Smart caching of results
- Minimal overhead on build system