# Poneglyph: A Graph Database for LLM-Driven Development

**TL;DR:** A hybrid graph database that combines compiler facts, build system knowledge, and LLM annotations to eliminate the "archaeology work" that LLMs spend 30-40% of their time doing when working with codebases.

---

## The Problem

Analysis of **31,500+ lines** of real LLM coding sessions across **3 OCaml projects** revealed that LLMs spend massive amounts of time on **build archaeology**:

- **322 grep commands** parsing build logs for errors
- **286 ls commands** checking for compiled artifacts  
- **68 find commands** searching for specific files
- **Parsing error messages manually** to understand missing dependencies
- **Multiple bash pipelines** like `grep | grep | tail | head` to extract structured information that the build system already knows

**Example from real session:**
```bash
# LLM trying to understand why linking failed:
grep "Error linking" build.log.68 | head -5
grep -B 5 "No implementations provided" build.log.68 | grep ocamlc | tail -1
grep "ocamlc.*-o tusk" build.log.68 | tail -1
find . -name "std_sys_unix*.o" 2>/dev/null
ls -la ./target/debug/cache/kernel/*.o 2>/dev/null
```

**All to answer:** "What modules are missing and where should they be?"

---

## The Solution

Poneglyph is a **daemon-maintained graph database** that:

1. **Auto-populates from build events** (via `tusk build` integration)
2. **Parses compiler output** (.cmt/.cmti files for OCaml semantics)
3. **Allows LLM annotations** (design decisions, patterns, bugs)
4. **Provides Datalog queries** for transitive relationships

### Architecture

```
┌─────────────────────────────────────────────────┐
│  tusk build                                      │
│  ├─ Compiles code                               │
│  ├─ Parses .cmt/.cmti files                     │
│  └─ Populates poneglyph graph                   │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│  poneglyph daemon                                │
│  ├─ Compiler facts (modules, types, deps)       │
│  ├─ Build facts (artifacts, errors, commands)   │
│  ├─ Git facts (changes, history)                │
│  └─ LLM annotations (patterns, decisions)        │
└─────────────────────────────────────────────────┘
                    ↓
┌─────────────────────────────────────────────────┐
│  LLM queries (via MCP or CLI)                    │
│  poneglyph query "?- missing(Build, File)."     │
└─────────────────────────────────────────────────┘
```

---

## What Gets Stored

### 1. Compiler Facts (from .cmt/.cmti files)
```datalog
edge("Std__Path", "defines_type", "Path.t")
edge("Std__Path", "exports_function", "Path.v")
edge("Std__Path", "depends_on", "Std__String")
edge("Tusk__Main", "references_undefined", "Std__Result")  # from linker errors
```

### 2. Build Facts (from tusk build)
```datalog
edge(Build_12345, "package", "tusk")
edge(Build_12345, "status", "failed")
edge(Build_12345, "error_type", "linker_error")
edge(Build_12345, "missing_symbols", ["Std__Result", "Std__Path"])
edge(Build_12345, "compiler_flags", ["-g", "-bin-annot", "-c"])
edge(Build_12345, "produced_files", [".cmi", ".cmx"])  # Missing .cmt!
```

### 3. Artifact Facts
```datalog
edge("Std__Path.cmx", "location", "target/debug/cache/abc123/")
edge("Std__Path.cmt", "hash", "sha256:...")
edge(Sandbox_xyz, "contains", "Tusk__Main.cmo")
edge(Cache_abc, "stores", "Std__Path.cmx")
```

### 4. Git Facts
```datalog
edge("packages/tusk/src/ocamlc.ml", "modified_at", "2024-10-05T09:54:00Z")
edge("packages/tusk/src/ocamlc.ml", "git_status", "modified")
edge("packages/tusk/src/ocamlc.ml", "last_commit", "abc123")
```

### 5. LLM Annotations (manually added)
```datalog
edge("tusk_jsonrpc.ml:event_loop", "pattern", "recursive-event-loop")
edge("tusk_jsonrpc.ml:event_loop", "exit_condition", "BuildCompleted message")
edge("build_server.ml:build_loop", "similar_to", "tusk_jsonrpc.ml:event_loop")
edge("concept:BuildCache", "bug", "issue:check-target-folder")
```

### 6. Semantic Mechanism Facts (from codebase analysis)
```datalog
# How mechanisms work:
edge("alias-module-compilation", "is_mechanism", true)
edge("alias-module-compilation", "prevents_problem", "circular-dependencies")
edge("alias-module-compilation", "implementation", "ocamldep.ml:111-113")
edge("alias-module-compilation", "key_insight", "forward-references-resolved-at-link-time")

# Compiler flags and their purposes:
edge("-no-alias-deps", "is_compiler_flag", true)
edge("-no-alias-deps", "purpose", "compile-alias-modules-without-targets")
edge("-no-alias-deps", "used_by", "Dune module wrapping")

# Type system introspection:
edge("Module_name", "is_build_system_type", true)
edge("Module_name", "has_method", "cmo")
edge("Module_name.cmo", "returns", "Path.t")
edge("Module_name.cmo", "purpose", "get-compiled-bytecode-filename")
```

### 7. Security Pattern Facts (auto-detected)
```datalog
edge("datalake.py:45", "contains_pattern", "aws-access-key")
edge("service.py:12", "contains_pattern", "postgresql-connection-string")
edge("aws-access-key", "security_risk", "high")
edge("postgresql-connection-string", "security_risk", "medium")
```

---

## Killer Queries (From Real Sessions)

### Query 1: "Why did package X fail?"
```datalog
?- edge(Build, "package", "tusk"),
   edge(Build, "status", "failed"),
   edge(Build, "error", Error),
   edge(Error, "missing_symbols", Missing).

# Instead of: grep "Error linking" build.log.68 | grep -A5 "No implementations"
```

### Query 2: "Where is module X compiled to?"
```datalog
?- edge(Module, "name", "Std__Path"),
   edge(Module, "compiled_to", Artifact),
   edge(Artifact, "location", Path).

# Instead of: find . -name "*Path*.o" 2>/dev/null
```

### Query 3: "What SHOULD have been produced vs what WAS produced?"
```datalog
expected(Build, File) :-
  edge(Build, "compiler_flags", Flags),
  member("-bin-annot", Flags),
  should_produce(Flags, File).

actual(Build, File) :-
  edge(Build, "produced", File).

?- expected(Build, File), not(actual(Build, File)).

# Returns: All .cmt files that should exist but don't!
```

### Query 4: "Show me all event loops and how they exit"
```datalog
?- edge(Fn, "pattern", "recursive-event-loop"),
   edge(Fn, "exit_condition", Cond),
   edge(Fn, "in_file", File).

# Returns: Similar code patterns across the codebase
```

### Query 5: "What changed since last successful build?"
```datalog
lastSuccess(Pkg, Build) :- 
  edge(Build, "package", Pkg),
  edge(Build, "status", "success"),
  not(exists(NewerBuild, newer(NewerBuild, Build))).

?- lastSuccess("tusk", LastBuild),
   currentBuild("tusk", CurrBuild),
   diff(LastBuild, CurrBuild, Changes).
```

### Query 6: "What uses Std.Path.t?"
```datalog
uses(Module, Type) :- edge(Module, "references", Type).
uses(Module, Type) :- edge(Module, "calls", Fn), uses(Fn, Type).

?- uses(Module, "type:Std.Path.t"), edge(Module, "in_package", Pkg).

# Instead of: grep -r "Path\.t" packages/ | grep -v ".cmi"
```

### Query 7: "How does mechanism X work?" (NEW - from Dune analysis)
```datalog
?- edge(Mechanism, "name", "alias-module-compilation"),
   edge(Mechanism, "prevents_problem", Problem),
   edge(Mechanism, "implementation", Files),
   edge(Mechanism, "key_insight", Insight).

# Instead of: 23 grep searches + reading 5+ files + connecting the dots manually
```

### Query 8: "Find all security issues" (NEW - from Data Insights)
```datalog
?- edge(File, "contains_pattern", Pattern),
   edge(Pattern, "security_risk", Level),
   Level = "high",
   edge(File, "git_status", "modified").

# Instead of: 16 different grep patterns run across entire codebase
```

---

## Implementation Plan

### Phase 1: Core Infrastructure (Week 1)
- [ ] Graph storage (in-memory + sqlite persistence)
- [ ] Basic entity/edge model
- [ ] CLI: `poneglyph set/link/get`
- [ ] Simple pattern queries (no Datalog yet)

### Phase 2: Build Integration (Week 2)  
- [ ] Hook into `tusk build` lifecycle
- [ ] Parse compiler output for errors
- [ ] Extract module dependencies from .cmt files
- [ ] Auto-populate: files, modules, compilation facts

### Phase 3: Datalog Engine (Week 3)
- [ ] Embed minimal Datalog (or use existing library)
- [ ] Transitive queries (depends_on, uses, blocks, etc.)
- [ ] Rule definitions for common patterns

### Phase 4: LLM Integration (Week 4)
- [ ] MCP server for poneglyph queries
- [ ] Auto-query on build failures
- [ ] Suggest queries based on context
- [ ] Pattern detection (event loops, similar code, etc.)

---

## Success Metrics

**Before Poneglyph (observed in real sessions):**
- 30+ bash commands to understand one linker error
- ~5 minutes of archaeology per bug
- LLM makes incorrect assumptions due to incomplete information

**After Poneglyph:**
- 1-3 Datalog queries to understand linker error
- ~30 seconds to get complete dependency graph
- LLM works with authoritative compiler facts

**Estimated time savings: 60-70% on debugging/archaeology tasks**

---

## Why This Beats LSP Alone

| Query | LSP | Poneglyph |
|-------|-----|-----------|
| "Where is X defined?" | ✅ Yes | ✅ Yes |
| "What uses X?" | ✅ Yes | ✅ Yes |
| "What pattern is this code?" | ❌ No | ✅ Yes |
| "Why did the build fail?" | ❌ No | ✅ Yes |
| "What's missing from the sandbox?" | ❌ No | ✅ Yes |
| "What changed since last build?" | ❌ No | ✅ Yes |
| "Show similar code patterns" | ❌ No | ✅ Yes |
| "What blocks feature X?" (transitive) | ❌ No | ✅ Yes |
| **"How does mechanism X work?"** | ❌ No | ✅ Yes |
| **"Why does compiler need flag Y?"** | ❌ No | ✅ Yes |
| **"Find security issues"** | ❌ No | ✅ Yes |

LSP provides **code navigation**. Poneglyph provides **semantic understanding** of build systems, mechanisms, and codebase structure.

---

## Why Now?

1. **Tusk exists** - we have a build system to integrate with
2. **.cmt files being added** - compiler facts will be available
3. **MCP integration planned** - natural query interface for LLMs
4. **Real data proves the need** - 30k lines of session logs show the pain

---

## Open Questions

1. **Schema evolution:** How do we version the graph schema?
2. **Performance:** In-memory graph size for large workspaces?
3. **Datalog library:** Build our own or embed existing (Soufflé, Flix, Crepe)?
4. **Query UX:** Natural language → Datalog translation?
5. **Stale data:** How aggressively to invalidate on file changes?

---

## Related Work

- **ocaml-lsp-server:** Code navigation (we complement, not replace)
- **Merlin:** Type-at-point (we use their .cmt parsing)
- **odoc:** Documentation (we could consume their dependency graphs)
- **Dune:** Build system (similar integration potential)

---

## Next Steps

1. ✅ Write this spec (you are here!)
2. ⬜ Design schema (entities, edges, attributes)
3. ⬜ Build minimal CLI (`set`, `link`, `get`)
4. ⬜ Integrate with `tusk build` to auto-populate
5. ⬜ Test with real build failures
6. ⬜ Add Datalog queries
7. ⬜ MCP server for LLM access

---

## Appendix: Real Query Patterns from LLM Sessions

This appendix documents actual queries observed across **31,500+ lines of LLM session logs** from multiple projects, showing what LLMs currently do vs. what Poneglyph would enable.

### Session Coverage
- **riot-ml/riot** (build system development): 22 sessions, 30,423 lines
- **ocaml-dune** (understanding Dune internals): 1 session, 204 entries
- **ocaml-data-insights** (Python/OCaml project): 2 sessions, 869 entries

### Tool Usage Aggregate
```
Total across all sessions:
- 3,189 Bash commands
- 2,106 Read operations  
- 1,273 Grep searches (41% of queries!)
- 2,536 Edit operations
- 735 TodoWrite operations

Archaeology commands (find/ls/grep): ~3,800+ operations
```

### A.1 Build Log Archaeology

#### Current Approach (Multiple Bash Commands):
```bash
# Parsing build logs to understand errors (observed 50+ times):
grep "Build.*completed" build.log.68 | tail -10
grep -E "(Successfully built|Failed to build)" build.log.68 | tail -20
grep -E "(Compiling package|Error linking|Error building)" build.log.68 | tail -20
grep "package" build.log.68 | grep -E "(kernel|std|jsonrpc|mcp|miniriot|tusk)" | tail -20
grep "Error linking" build.log.68 | head -5
grep -A 5 "No implementations provided" build.log.68 | head -20
```

#### Poneglyph Approach (Single Query):
```datalog
?- edge(Build, "timestamp", T), T = latest(),
   edge(Build, "status", Status),
   edge(Build, "package", Package),
   (Status = "failed" -> edge(Build, "error", Error) ; true).

# Returns structured:
# Build_12345: {package: "tusk", status: "failed", error: "linker_error", 
#               missing: ["Std__Result", "Std__Path"]}
```

**Time saved:** ~2 minutes → ~10 seconds

---

### A.2 Finding Compiled Artifacts

#### Current Approach:
```bash
# Looking for where files were compiled (observed 40+ times):
ls -la ./target/debug/cache/kernel/*.o 2>/dev/null
ls -la ./target/bootstrap/out/kernel/*.o 2>/dev/null
find . -name "std_sys_unix*.o" 2>/dev/null | head -5
ls ./target/debug/cache/kernel/*/
ls -lt target/bootstrap/sandbox/*/std.cma | head -1
find target/debug/sandbox/0e905960 -name "*.cmt*" | head -20
```

#### Poneglyph Approach:
```datalog
?- edge(Module, "name", "Std_sys_unix"),
   edge(Module, "compiled_to", Artifacts),
   edge(Artifacts, "location", Path).

# Or for all artifacts of a package:
?- edge(Package, "name", "kernel"),
   edge(Package, "artifacts", Files),
   edge(Files, "path", Paths).
```

**Time saved:** ~1 minute → ~5 seconds

---

### A.3 Understanding Module Dependencies

#### Current Approach:
```bash
# Extracting dependency information from linker errors:
grep "Std__Result referenced from" build.log | head -20
grep "Std__Path referenced from Tusk__Actions" build.log
grep -E "unresolved.*MutIterator|Cannot.*MutIterator" build.log.59
grep "MutIterator" build.log.59 | head -20
```

#### Poneglyph Approach:
```datalog
# Direct dependency query:
?- edge("Tusk__Actions", "references", Module),
   edge(Module, "status", "undefined").

# Or transitive dependencies:
depends(X, Y) :- edge(X, "references", Y).
depends(X, Z) :- depends(X, Y), depends(Y, Z).

?- depends("Tusk__Main", What), edge(What, "status", "undefined").
```

**Time saved:** ~3 minutes → ~15 seconds

---

### A.4 Code Pattern Search

#### Current Approach:
```bash
# Finding similar code patterns (observed 25+ times):
grep "topological|topo_sort|sort.*packages" packages/
grep "topological_sort.*graph" packages/
grep "let run|let analyze" packages/
grep "CreateExecutable.*libraries" packages/
grep "ocamldep.*-sort|sort_dependencies" packages/
```

#### Poneglyph Approach:
```datalog
?- edge(Function, "implements_algorithm", "topological-sort"),
   edge(Function, "in_file", File),
   edge(Function, "line_range", Lines).

# Or find similar patterns:
?- edge(Fn1, "pattern", "recursive-event-loop"),
   edge(Fn2, "pattern", "recursive-event-loop"),
   Fn1 != Fn2,
   edge(Fn1, "exit_strategy", S1),
   edge(Fn2, "exit_strategy", S2).
```

**Time saved:** ~2 minutes → ~10 seconds

---

### A.5 Git/File History Queries

#### Current Approach:
```bash
# Understanding what changed (observed 30+ times):
git status --short
git log --oneline -5
git diff --cached --stat
git status packages/
git diff packages/tusk/src/core/module_graph.ml | grep -E "^[+-].*Printf" | head -20
```

#### Poneglyph Approach:
```datalog
?- edge(File, "modified_at", Date),
   Date > now() - hours(24),
   edge(File, "git_status", Status),
   edge(File, "in_package", Package).

# Or diff between builds:
?- edge(Build_old, "timestamp", T1),
   edge(Build_new, "timestamp", T2),
   T2 > T1,
   edge(Build_old, "compiled", Files_old),
   edge(Build_new, "compiled", Files_new),
   diff(Files_old, Files_new).
```

**Time saved:** ~1 minute → ~10 seconds

---

### A.6 Checking Compilation Commands

#### Current Approach:
```bash
# Finding what compilation command was used:
grep "ocamlc.*-o tusk" build.log.68 | tail -1
grep -B 5 "No implementations provided" build.log.68 | grep ocamlc | tail -1
./target/debug/tusk build -p jsonrpc 2>&1 | grep -A2 -B2 "bin-annot" | head -20
```

#### Poneglyph Approach:
```datalog
?- edge(Build, "package", "tusk"),
   edge(Build, "link_command", Cmd).

# Or check if specific flags were used:
?- edge(Build, "package", "std"),
   edge(Build, "compiler_flags", Flags),
   member("-bin-annot", Flags).
```

**Time saved:** ~1 minute → ~5 seconds

---

### A.7 Understanding Build System Types

#### Current Approach:
```bash
# LLM reading code to understand the build system's own types:
Search(pattern: "\.cmi|\.cmx|\.cmo", path: "packages/tusk/src")  # 10 files
Search(pattern: "DeclareOutputs")  # 6 files
Read(packages/tusk/src/model/module_name.ml)  # Looking at cmo(), cmi() functions
```

#### Poneglyph Approach:
```datalog
# Introspect the build system itself:
?- edge(Type, "name", "Module_name"),
   edge(Type, "has_method", Method),
   edge(Method, "returns", "file-extension").

# Returns: [cmo, cmi, cmx, o, a, canonical_mli, cmt, cmti]
```

**Time saved:** ~3 minutes reading code → ~5 seconds

---

### A.8 Debugging Missing Outputs

#### Current Approach:
```bash
# Checking if expected files were produced (observed 15+ times):
find target/debug/out -name "*.cmt" -o -name "*.cmti" | head -30
find target/debug/sandbox/0e905960 -name "*.cmt*" | head -20
find target/debug/cache -name "*.cmt*" | head -20
ls target/debug/sandbox/0e905960/*.cmt* 2>/dev/null

# Then manually checking manifest:
cat target/debug/cache/036125dae.../manifest.json | grep "\.cmt"
```

#### Poneglyph Approach:
```datalog
# Expected vs actual output check:
expected(Build, File) :-
  edge(Build, "compiler_flags", Flags),
  member("-bin-annot", Flags),
  edge(Build, "source", Source),
  suffix(Source, ".ml"),
  cmt_from_source(Source, File).

actual(Build, File) :-
  edge(Build, "sandbox", Sandbox),
  edge(Sandbox, "contains", File).

missing(Build, File) :-
  expected(Build, File),
  not(actual(Build, File)).

?- missing(Build, File).
# Returns: All .cmt files that should exist but don't!
```

**Time saved:** ~5 minutes → ~15 seconds

---

### A.9 Finding Where to Add Code

#### Current Approach:
```bash
# Finding where compiler flags are set (observed in .cmt session):
Search(pattern: "ocamlc.*-c")          # 192 files (too many!)
Search(pattern: "promote|copy.*\.cm")   # 329 files (noise!)
Search(pattern: "\.cmo|\.cmi|\.cmx")   # 732 files (massive noise!)

# Then narrowing down:
Search(pattern: "DeclareOutputs")      # 6 files
Search(pattern: "\.cmi|\.cmx|\.cmo", path: "packages/tusk/src")  # 10 files
```

#### Poneglyph Approach:
```datalog
# Semantic query for build system roles:
?- edge(File, "role", "compiler-wrapper"),
   edge(File, "configures", "compilation-flags").

# Returns: packages/tusk/src/ocaml/ocamlc.ml

# Or for output management:
?- edge(File, "manages", "build-outputs"),
   edge(File, "in_package", "tusk").

# Returns: packages/tusk/src/core/module_graph.ml
```

**Time saved:** ~10 minutes searching → ~30 seconds

---

### A.10 Binary Version Confusion

#### Current Approach:
```bash
# Debugging which binary is being used (observed in .cmt session):
which tusk                               # /Users/ostera/.tusk/bin/tusk
./tusk build                             # Uses installed version (wrong!)
./target/debug/tusk build                # Uses just-built version (correct!)
ls -la ./target/debug/tusk 2>/dev/null  # Check if it exists
```

#### Poneglyph Approach:
```datalog
# Track multiple binary versions:
?- edge(Binary, "path", "/Users/ostera/.tusk/bin/tusk"),
   edge(Binary, "built_at", Date1),
   edge(LocalBinary, "path", "./target/debug/tusk"),
   edge(LocalBinary, "built_at", Date2),
   edge(LocalBinary, "has_changes", Changes),
   Date2 > Date1.

# Alert: "Local binary is newer and has changes: [ocamlc.ml, module_graph.ml]"
```

**Time saved:** Prevents debugging with wrong binary (priceless!)

---

### A.11 Understanding Control Flow

#### Current Approach:
```bash
# LLM reading code to understand control flow (bug-fixing session):
Read(packages/tusk/src/server/tusk_jsonrpc.ml, offset: 2270, limit: 40)
# Looking for: where does event_loop recurse? When does it exit?

Search(pattern: "event_loop")
# Then manual reading to understand structure
```

#### Poneglyph Approach:
```datalog
# Query control flow patterns:
?- edge(Function, "name", "event_loop"),
   edge(Function, "pattern", "recursive"),
   edge(Function, "recursive_call", Location),
   edge(Function, "exit_condition", Condition).

# Or find similar patterns:
?- edge(Fn1, "pattern", "recursive-event-loop"),
   edge(Fn2, "similar_to", Fn1),
   edge(Fn1, "exit_strategy", S1),
   edge(Fn2, "exit_strategy", S2).
```

**Time saved:** ~5 minutes reading → ~20 seconds

---

### A.12 Transitive Blocking Queries

#### Current Approach:
```bash
# No good way to do this with grep/find!
# LLM has to manually read TODOS.md and trace dependencies by hand

# Example: "What blocks incremental builds?"
# 1. Read TODOS.md
# 2. Find "incremental-builds" blocked by "crypto-module-bindings"
# 3. Read crypto module issue
# 4. Find it needs "std-sys/crypto" package
# 5. Check if package exists
# (This took ~10 minutes in actual session)
```

#### Poneglyph Approach:
```datalog
# Transitive blocking:
blocks(X, Y) :- edge(X, "blocks", Y).
blocks(X, Z) :- blocks(X, Y), blocks(Y, Z).

?- blocks(What, "feature:incremental-builds").

# Returns entire blocking chain:
# issue:crypto-module-bindings → 
# package:std-sys/crypto (missing) → 
# feature:incremental-builds
```

**Time saved:** ~10 minutes → ~10 seconds

---

### A.13 Semantic Code Understanding (New from Dune Session)

#### Current Approach:
```bash
# Understanding "How does mechanism X work?" (23 grep searches):
grep "wrapped.*module|module.*wrap|namespace|alias.*module"
grep "Module_name\.wrap|wrapped_compat|Wrapped"
grep "alias_module_build|gen_alias_module|generate_alias"
grep "Wrapped\\.Simple|wrapped.*transition"
grep "type.*Wrapped|Wrapped.*="
grep "Simple true|Simple false|Yes_with_transition"
grep "Module\.Kind\.Alias|kind.*=.*Alias"
grep "deps.*Alias|dependencies.*Alias|ocamldep"
grep "-open.*Alias|-open.*module"
grep "open_modules|opens|Module\.opens"
grep "no-alias-deps|alias.*deps"
grep "Ml_kind\.(Impl|Intf)|ml_kind.*Intf"
# ... 11 more greps to understand one mechanism!
```

**The question:** "How does Dune avoid circular dependencies with alias modules?"

**The archaeology:**
1. Grep for `alias.*module` → find relevant files
2. Read `modules.ml` → understand wrapping  
3. Read `module_compilation.ml` → see alias generation
4. Grep for `ocamldep` → find dependency tracking
5. Read `ocamldep.ml:111-113` → discover alias modules are SKIPPED
6. Connect the dots: Alias modules use forward references resolved at link time

#### Poneglyph Approach:
```datalog
# Query the mechanism directly:
?- edge(Mechanism, "name", "alias-module-compilation"),
   edge(Mechanism, "prevents_problem", Problem),
   edge(Mechanism, "implementation_files", Files),
   edge(Mechanism, "key_insight", Insight).

# Returns:
# Problem: "circular-dependencies"
# Files: ["ocamldep.ml:111-113", "module_compilation.ml:383-449"]
# Insight: "Alias modules are skipped in dependency tracking because
#           they use forward references resolved at link time, not compile time"
```

**Time saved:** ~15 minutes of detective work → ~30 seconds

---

### A.14 Security Audit Queries (From Data Insights Session)

#### Current Approach:
```bash
# Finding credential exposures (16 grep searches):
grep "password.*=.*['\"]|PASSWORD.*=.*['\"]|secret.*=.*['\"]"
grep "ghp_|ghs_|github.com/[a-zA-Z0-9]+:[a-zA-Z0-9]+@"
grep "AKIA[0-9A-Z]{16}|ASIA[0-9A-Z]{16}"  # AWS keys
grep "postgres://.*@|postgresql://.*@"
grep "jwt[_-]?secret|private[_-]?key"
grep "BEGIN RSA|BEGIN PRIVATE"
# ... searching across entire codebase for credential patterns
```

#### Poneglyph Approach:
```datalog
# Auto-indexed security patterns:
?- edge(File, "contains_pattern", Pattern),
   edge(Pattern, "security_risk", "high"),
   edge(File, "modified_at", Date),
   Date > now() - days(1).

# Or check before commit:
?- edge(File, "git_status", "modified"),
   edge(File, "contains_credential", Cred),
   edge(Cred, "type", Type).

# Alert: "Files contain credentials: 
#   - datalake.py: AWS_ACCESS_KEY_ID
#   - service.py: postgresql://user:pass@host"
```

**Time saved:** ~10 minutes manual grepping → ~10 seconds query + automatic alerts

---

### A.15 Type System Introspection (Cross-Project Pattern)

All three projects showed LLMs trying to understand the **codebase's own type system**:

#### Current Approach:
```bash
# Riot ML: "What file types does Module_name generate?"
Search(pattern: "\.cmo|\.cmi|\.cmx", path: "packages/tusk/src")  # 10 files
Read(packages/tusk/src/model/module_name.ml)  # Looking for cmo(), cmi(), etc.

# Dune: "What types exist in the wrapping system?"
grep "type.*Wrapped|Wrapped.*="
grep "Module.Kind|type.*Kind"
grep "Ml_kind\.(Impl|Intf)"

# Data Insights: "What classes exist in ingestion?"
grep "^class.*Loader|^class.*Scraper" ./ingestion
```

#### Poneglyph Approach:
```datalog
# For build systems - introspect their internal types:
?- edge(Type, "name", "Module_name"),
   edge(Type, "is_build_system_type", true),
   edge(Type, "has_method", Method),
   edge(Method, "purpose", Purpose).

# Returns:
# Module_name.cmo → "get compiled bytecode filename"
# Module_name.cmi → "get compiled interface filename"
# Module_name.cmx → "get compiled native code filename"
# Module_name.cmt → "get typed tree annotation filename"

# For any codebase - track class hierarchies:
?- edge(Class, "is_class", true),
   edge(Class, "matches_pattern", ".*Loader"),
   edge(Class, "inherits_from", Parent),
   edge(Class, "implements_pattern", Pattern).

# Returns:
# GitHubLoader → inherits: BaseLoader → pattern: data-ingestion
# OPAMLoader → inherits: BaseLoader → pattern: data-ingestion
```

**Time saved:** ~5 minutes code reading → ~15 seconds

---

### A.16 Understanding Compiler Mechanisms (Dune-Specific)

The Dune session revealed LLMs need to understand **"why the compiler does what it does"**:

#### Current Approach:
```bash
# Testing compiler behavior manually:
cd /tmp/alias_test
ocamlc -c std__data.mli && ocamlc -c std__data.ml
ocamlc -c -no-alias-deps std.ml
ocamlc -c test.ml  # Does it work?

# Then grepping to understand the mechanism:
grep "no-alias-deps" # What is this flag?
grep "alias.*dependencies|Alias.*deps"
grep "open_modules|opens|Module\.opens"
```

**The investigation:** "Why does `-no-alias-deps` exist and what does it prevent?"

#### Poneglyph Approach:
```datalog
# Query compiler flag semantics:
?- edge(Flag, "name", "-no-alias-deps"),
   edge(Flag, "purpose", Purpose),
   edge(Flag, "prevents", Problem),
   edge(Flag, "used_by", Mechanism).

# Returns:
# Purpose: "Compile alias modules without requiring their targets"
# Prevents: "circular-dependency during alias module compilation"
# Used by: "Dune's module wrapping system"
# Context: "Alias modules contain forward references, resolved at link time"
```

**Time saved:** ~20 minutes experimentation + reading → ~30 seconds

---

## Summary Statistics

### Aggregated Across All Projects

**Total tool usage:**
- **3,189 Bash commands**
  - Grep: 1,273 (40%)
  - Ls/find: ~500 (16%)
  - Git: 411 (13%)
  - Build commands: 807 (25%)
- **2,106 Read operations** (many to understand types/mechanisms)
- **1,273 Grep searches** (pattern hunting)
- **~3,800 total archaeology operations**

**Time breakdown (estimated):**
- 30-40% of session time spent on archaeology
- 15-25 bash commands per debugging task
- 5-15 minutes per "understanding how X works" task

**With Poneglyph (projected):**
- 2-5 Datalog queries replace most bash archaeology
- 85% reduction in archaeology operations
- 60-70% time savings on debugging/comprehension tasks

### Key Findings by Project Type

| Project Type | Main Pattern | Query Count | Top Need |
|--------------|--------------|-------------|----------|
| **Build System** (Riot) | Build archaeology | 3,102 bash | "Why did build fail?" |
| **Codebase Study** (Dune) | Mechanism understanding | 23 greps | "How does X work?" |
| **Mixed** (Data Insights) | Structure + security | 45 find/ls/grep | "What exists + is it safe?" |

### Novel Query Categories Discovered

Beyond the original 12 query patterns, cross-project analysis revealed:

**A.13 - Semantic Code Understanding:** Understanding mechanisms, not just locations (23 greps → 1 query)

**A.14 - Security Auditing:** Finding credentials/secrets across codebase (16 greps → 1 query + auto-alerts)

**A.15 - Type System Introspection:** Understanding the codebase's own type system (10+ greps + reads → 1 query)

**A.16 - Compiler Behavior Investigation:** Why does the compiler do X? (experimentation + reading → semantic query)

---

## Critical Insight: It's Not Just Build Archaeology

While build archaeology remains the #1 time sink, the extended analysis reveals **three distinct use cases** for Poneglyph:

### 1. **Build Archaeology** (Riot sessions)
- "What failed and why?"
- "Where are the artifacts?"
- "What's the dependency chain?"

### 2. **Semantic Code Comprehension** (Dune session)
- "How does mechanism X work?"
- "Why does the compiler need flag Y?"
- "What's the relationship between A and B?"

### 3. **Codebase Auditing** (Data Insights session)
- "What structure exists?" (classes, modules, patterns)
- "Are there security issues?"
- "What patterns are used where?"

**Poneglyph must serve all three use cases** - not just build systems, but any codebase where LLMs need to understand structure, semantics, and relationships.

---

**Let's build the graph database that LLMs wish they had.**
