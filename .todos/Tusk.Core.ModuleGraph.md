## Module Naming and Normalization

### REQ-001: Module Name Capitalization from String
- **Given** a string identifier
- **Example**: "myModule" → "MyModule", "hTTP" → "HTTP"
- **Must** capitalize only the first character
- **Must** preserve the casing of all other characters
- **Must** handle empty strings by returning them unchanged

### REQ-002: Module Name from File Path
- **Given** a file path (e.g., "/path/to/my_file.ml")
- **Example**: "/src/http_client.ml" → "Http_client"
- **Must** extract the basename without extension
- **Must** apply REQ-001 capitalization rules
- **Must** handle paths with or without extensions

### REQ-003: Archive Name Generation
- **Given** a module name
- **Example**: "Kernel" → "Kernel.cma"
- **Must** generate archive name by appending ".cma"

## Namespace Management

### REQ-004: Namespace Separator Convention
- **Given** namespace parts ["Std", "Net", "Http"]
- **Example**: ["Std", "Net", "Http"] → "Std__Net__Http"
- **Must** use double underscore "__" as separator
- **Must** match OCaml compiler convention for flat namespace compilation

### REQ-005: Empty Namespace Representation
- **Given** a namespace structure
- **Example**: Empty namespace → empty list []
- **Must** represent empty namespace as empty list
- **Must** support checking if namespace is empty
- **Must** convert empty namespace to empty string

### REQ-006: Namespace Construction and Extension
- **Given** an existing namespace and new module name
- **Example**: Namespace ["Std", "Net"] + "Http" → ["Std", "Net", "Http"]
- **Must** support creating namespace from list of module names
- **Must** support appending module names to existing namespace
- **Must** preserve order of parts

### REQ-007: Namespace to String Conversion
- **Given** a namespace with parts
- **Example**: ["Std", "Net", "Http"] → "Std__Net__Http"
- **Must** join all parts with "__" separator
- **Must** return empty string for empty namespace

## Module Identity and Metadata

### REQ-008: Module File Type Recognition
- **Given** a file with extension
- **Example**: "foo.mli" → interface, "foo.ml" → implementation
- **Must** recognize ".ml" as implementation
- **Must** recognize ".mli" as interface
- **Must** recognize ".c" as C source
- **Must** recognize ".h" as C header
- **Must** fail for unrecognized OCaml file extensions

### REQ-009: Module Creation from Path and Namespace
- **Given** a file path and namespace
- **Example**: path="src/client.ml", ns=["Std", "Net"] → module "Std__Net__Client"
- **Must** extract module name from file path
- **Must** combine namespace with module name
- **Must** store original file path
- **Must** compute namespaced name

### REQ-010: Module Equality by File Path
- **Given** two module instances
- **Example**: Module("src/foo.ml") = Module("src/foo.ml"), even if names differ
- **Must** consider modules equal if and only if file paths match
- **Must not** use module name alone for equality
- **Must** treat file path as source of truth

### REQ-011: Compiled Artifact Naming
- **Given** a module with namespaced name
- **Example**: "std/net/http.ml" -> "Std__Net__Http" → "Std__Net__Http.cmi", "Std__Net__Http.cmo"
- **Must** generate .cmi filename as "{namespaced_name}.cmi"
- **Must** generate .cmo filename as "{namespaced_name}.cmo"

### REQ-012: Special Aliases Module Recognition
- **Given** a folder inside of a package
- **Example**: pkg/src/mylib
- **Must** create a Pkg__Mylib__Aliases.ml.gen file
- **Must** have this module re-expose all modules within mylib (not recursively)
- **Example**: pkg/src/mylib/a.ml -> ends up in Pkg__Mylib__Aliases.ml.gen as "module A = Pkg__Mylib__A"

## File Representation

### REQ-013: Concrete File Representation
- **Given** a file that exists on filesystem
- **Example**: Concrete("src/foo.ml") represents actual file
- **Must** represent files that exist on disk
- **Must** store the file path
- **Must** be used for user-written source files

### REQ-014: Generated File Representation
- **Given** a file to be created during build
- **Example**: Generated{path="Std__Aliases.ml.gen", contents="module Foo = Std__Foo"}
- **Must** represent files to be generated during build
- **Must** store the target file path
- **Must** store the contents to be written
- **Must** be distinguishable from concrete files

## Build Results Registry (Cross-Package Dependencies)

### REQ-015: Package Registration with Outputs
- **Given** a package name, module name, and output artifacts
- **Example**: register("kernel", "Kernel", ["kernel.cmi", "kernel.cmo"])
- **Must** register package in build results by module name (not package name string)
- **Must** track registration order in a list
- **Must** associate outputs with the package
- **Must** preserve registration order for deterministic builds

### REQ-016: Module Existence Check in Build Results
- **Given** a module name to check
- **Example**: has_module("Kernel") → true if previously registered
- **Must** check if any registered package provides this module
- **Must** use module name for lookup

### REQ-017: Dependency Copying to Sandbox
- **Given** a sandbox directory path
- **Example**: copy all registered .cmi/.cmo files to sandbox/
- **Must** copy all registered outputs to sandbox
- **Must** use basename only for destination (flatten structure)
- **Must** skip files that don't exist
- **Must** log which files are copied with "Copied dependency" message
- **Must** enable flat -I include paths during compilation

### REQ-018: Package Name Listing
- **Given** a populated build results registry
- **Example**: get_package_names() → ["Kernel", "Std", "Net"] in order
- **Must** return list of all registered package module names
- **Must** preserve registration order
- **Must** enable dependency list computation

## Module Registry (Intra-Package)

### REQ-019: Module Registration with Node ID
- **Given** a module and graph node ID
- **Example**: register(module="Foo", node_id=5) creates bidirectional mapping
- **Must** store bidirectional mapping between node ID and module
- **Must** maintain separate hash tables for interfaces and implementations
- **Must** allow same module name to map to both interface and implementation nodes

### REQ-020: Module Lookup by Node ID
- **Given** a graph node ID
- **Example**: get(node_id=5) → Module("src/foo.ml")
- **Must** return the associated module
- **Must** raise Not_found if node ID not in registry

### REQ-021: Module Lookup by Name Returns List
- **Given** a module name
- **Example**: get_by_name("Foo") → [node_id_3, node_id_5] (interface and impl)
- **Must** return list of 0, 1, or 2 node IDs
- **Must** include interface node first if exists
- **Must** include implementation node second if exists
- **Must** return empty list and raise Not_found if not found
- **Must** handle modules with only .ml, only .mli, or both

## Dependency Graph Structure

### REQ-022: Graph Node Contents
- **Given** a graph node being created
- **Example**: node = {file=Concrete("foo.ml"), open_modules=[alias_node], kind=ML}
- **Must** store file representation (concrete or generated)
- **Must** store list of modules to open during compilation (open_modules)
- **Must** store node kind: ML, MLI, C, H, Other, or Root
- **Must** allow open_modules to be mutable for graph construction

### REQ-023: Root Node Singleton
- **Given** a dependency graph
- **Example**: graph has exactly one root node with kind=Root
- **Must** have exactly one root node per graph
- **Must** use root node as parent for top-level package modules
- **Must** skip root node during iteration and build

### REQ-024: Edge Semantics for Build Order
- **Given** an edge from node A to node B
- **Example**: foo.ml → foo.mli means "foo.ml depends on foo.mli"
- **Must** mean "A depends on B"
- **Must** mean "B must be built before A"
- **Must** enforce build ordering via topological sort

### REQ-025: Mutable open_modules Field
- **Given** a dep record during graph construction
- **Example**: node.open_modules updated as aliases discovered
- **Must** have mutable open_modules field in dep record
- **Must** allow updating which modules to open during graph construction
- **Must** accept side effects during graph building

## Alias Module Generation

### REQ-026: Alias Module Purpose and Content
- **Given** a list of child modules in a namespace
- **Example**: children [Client, Server] → "module Client = Pkg__Client\nmodule Server = Pkg__Server"
- **Must** generate Aliases module that re-exports all children
- **Must** include only implementation files (filter out interfaces)
- **Must** deduplicate by module name (sort_uniq)
- **Must** generate: "module {ShortName} = {Namespaced_Name}"
- **Must** sort entries alphabetically for deterministic output

### REQ-027: Alias Module Filename Convention
- **Given** a namespace like ["Std", "Net"]
- **Example**: "Std__Net__Aliases.ml.gen"
- **Must** generate filename: "{Namespace}__Aliases.ml.gen"
- **Must** use .gen extension to mark as generated

### REQ-028: Alias Module Header Comment
- **Given** generated alias module contents
- **Example**: First line is "(* Alias module generated by tusk *)"
- **Must** include comment: "(* Alias module generated by tusk *)"
- **Must** place header as first line of generated file

### REQ-029: Alias Module Deduplication Logic
- **Given** child modules with both .ml and .mli files
- **Example**: Foo.ml and Foo.mli → only one "module Foo = Pkg__Foo" alias
- **Must** deduplicate by module name
- **Must** prevent module appearing twice (once for .ml, once for .mli)

## Library Interface Generation

### REQ-030: Library Interface Purpose and Content
- **Given** a directory with child modules
- **Example**: directory "net" with [Client, Server] → "module Client = Client\nmodule Server = Server"
- **Must** generate parent module that re-exports children
- **Must** deduplicate child modules by name
- **Must** generate: "module {Name} = {Name}" for each child
- **Must** sort entries alphabetically for deterministic output

### REQ-031: Library Interface Opt-In User Control
- **Given** a directory "foo" with potential foo.ml or foo.mli
- **Example**: If src/net/net.ml exists, use it; otherwise generate
- **When** user provides their own {lib}.ml or {lib}.mli file
- **Must** use the user's file instead of generating
- **Must** mark as concrete file, not generated
- **Must** use case-insensitive module name matching for detection

### REQ-032: Library Interface Header Comment
- **Given** generated library interface contents
- **Example**: First line is "(* Library interface module generated by tusk *)"
- **Must** include comment: "(* Library interface module generated by tusk *)"
- **Must** place header as first line

### REQ-033: Library Interface Actual File Path
- **Given** library interface that exists on disk
- **Example**: Use actual path "src/net/net.ml" not constructed path
- **When** user provides hand-written library file
- **Must** extract and use actual file path from file tree
- **Must** not use constructed path for existing files

## Hierarchical Dependency Rules

### REQ-034: Namespace Parsing for Dependency Rules
- **Given** a namespaced module name like "Std__Net__Http__Client"
- **Example**: "Std__Net__Http__Client" → ["Std", "Net", "Http", "Client"]
- **Must** parse into parts by splitting on underscore
- **Must** filter out empty strings from split result

### REQ-035: Self-Dependency Rejection
- **Given** module A depending on module A
- **Example**: Std__Net__Http depending on Std__Net__Http
- **When** from_module equals to_module
- **Must** reject as not a real dependency
- **Must** return false from is_dependency_allowed

### REQ-036: Sibling Dependencies Allowed
- **Given** modules with same prefix, different last element
- **Example**: Std__Net__Http__Request → Std__Net__Http__Response
- **When** all parts except last are identical
- **Must** allow dependency
- **Must** return true from is_dependency_allowed

### REQ-037: Parent-to-Child Dependencies Allowed
- **Given** dependency where target has prefix equal to source
- **Example**: Std__Net → Std__Net__Http (parent Net can use child Http)
- **When** to_parts starts with from_parts and is longer
- **Must** allow dependency
- **Must** enable parent modules to depend on their children

### REQ-038: Child-to-Parent Dependencies Forbidden
- **Given** dependency where source has prefix equal to target
- **Example**: Std__Net__Http__Client → Std__Net__Http
- **When** child depends on parent
- **Must** reject dependency
- **Must** prevent circular dependencies and enforce layered architecture

### REQ-039: Cousin Dependencies (Ancestor's Sibling) Allowed
- **Given** deep module depending on ancestor's sibling
- **Example**: Std__Net__Http__Client__Pool → Std__Net__Server
- **When** target is exactly one level below common ancestor
- **When** source is more than one level below common ancestor
- **Must** allow dependency
- **Must** compute longest common prefix
- **Must** verify to_parts length = common + 1
- **Must** verify from_parts length > common + 1

## File System Scanning

### REQ-040: Source Root Discovery Convention
- **Given** a package root directory
- **Example**: packages/kernel → src_root is packages/kernel/src
- **Must** use {root}/src as source root
- **Must** follow convention that src/ is implicit root

### REQ-041: Recursive File Tree Walking
- **Given** a source directory
- **Example**: Walk packages/kernel/src recursively
- **Must** recursively traverse source directory
- **Must** identify .ml, .mli, .c, .h files
- **Must** identify subdirectories as potential library modules
- **Must** build file tree structure

### REQ-042: File Extension Recognition and Dispatch
- **Given** a file with extension during scanning
- **Example**: .mli dispatches to handle_ocaml_module, .c to handle_c_file
- **Must** recognize ".ml" as implementation → handle_ocaml_module
- **Must** recognize ".mli" as interface → handle_ocaml_module
- **Must** recognize ".c" as C source → handle_c_file
- **Must** recognize ".h" as C header → handle_h_file
- **Must** skip unrecognized extensions with log message

## Graph Construction - Directory Handling

### REQ-043: Root Package Library Naming
- **Given** root directory where namespace is empty
- **Example**: packages/kernel/src → "Kernel" module, not "Src"
- **Must** use package name for library module at root
- **Must not** use directory name "src"
- **Must** rename directory name to package name for library interface

### REQ-044: Nested Library Naming
- **Given** subdirectory where namespace is non-empty
- **Example**: packages/std/src/net → "Std__Net" module
- **Must** use directory name for nested library module
- **Must** extend namespace with directory's capitalized name

### REQ-045: Library File Detection by Module Name
- **Given** a directory named "foo"
- **Example**: Check if foo.ml or Foo.ml exists using module name match
- **Must** check for {dirname}.ml and {dirname}.mli as library interface files
- **Must** use case-insensitive module name matching (not path matching)
- **Must** extract actual file path if found
- **Must** track separately for .ml and .mli

### REQ-046: Library File Exclusion from Children
- **Given** children of a library directory
- **Example**: If directory is "net", exclude net.ml and net.mli from children
- **Must** exclude files with same module name as library from children list
- **Must** process remaining files as library contents
- **Must** prevent library interface files from being processed as regular children

### REQ-047: Directory vs File Module Precedence
- **Given** both directory "foo/" and file "foo.ml" exist
- **Example**: foo.ml takes precedence, skip foo/ as module
- **Must** use file module only
- **Must** skip directory module if file exists
- **Must** prevent duplicate modules with same name

### REQ-048: Synthetic Directory Modules for Aliases
- **Given** subdirectory with no corresponding file module
- **Example**: Directory "http/" with no http.ml → create synthetic module for alias
- **When** no file module with same name exists
- **Must** create synthetic module from directory name with .ml path
- **Must** include in alias generation
- **Must** not register as real module in registry
- **Must** only use for exposing sub-namespace

## Graph Construction - Module Handling

### REQ-049: OCaml Module Node Creation
- **Given** an .ml or .mli file during scanning
- **Example**: src/client.ml with namespace ["Pkg"] → node for Pkg__Client
- **Must** create module from file path and namespace
- **Must** create graph node with concrete file representation
- **Must** register module in registry with node ID
- **Must** add current aliases to open_modules list
- **Must** set kind to ML or MLI based on extension

### REQ-050: Implementation-to-Interface Edge
- **Given** an implementation file being processed
- **Example**: foo.ml finds foo.mli in registry → add edge foo.ml → foo.mli
- **When** corresponding interface exists in registry with same module name
- **Must** look up module name in registry
- **Must** add edge from implementation node to interface node
- **Must** ensure .cmo file waits for .cmi file

### REQ-051: Interface-First Processing Order
- **Given** children of a directory
- **Example**: Process [foo.mli, bar.mli, foo.ml, bar.ml] in that order
- **Must** process .mli files before .ml files
- **Must** sort children with comparator: .mli < .ml < others
- **Must** enable implementation files to find their interfaces in registry
- **Must** not rely on filesystem order

### REQ-052: Parent Linking by File Kind
- **Given** a module being added to graph
- **Example**: foo.mli links to parent_intf, foo.ml links to parent_impl
- **Must** link interface files to parent interface node
- **Must** link implementation files to parent implementation node
- **Must** add edge from parent to child

### REQ-053: Alias Linking for All Modules
- **Given** a module node with aliases in scope
- **Example**: Module in net/http/ opens [Std__Aliases, Std__Net__Aliases]
- **Must** add edges from module node to all alias nodes in scope
- **Must** enable compiler to open aliases with -open flag

## Graph Construction - C File Handling

### REQ-054: C File Dependency on Parent Implementation
- **Given** a .c file in a directory
- **Example**: src/sha256.c → parent_impl depends on sha256.c node
- **Must** create graph node with concrete file and kind=C
- **Must** add edge from parent implementation to C file node
- **Must not** register in module registry
- **Must** ensure C files compiled before OCaml implementation

### REQ-055: Header File Copy-Only Node
- **Given** a .h file in a directory
- **Example**: src/sha256.h → node with no edges
- **Must** create graph node with concrete file and kind=H
- **Must not** add any dependency edges
- **Must not** register in module registry
- **Must** treat as file to be copied, not compiled
- **Must** not affect build order

## Graph Construction - Library Structure

### REQ-056: Alias Module Creation First
- **Given** a library being processed with children
- **Example**: Create Pkg__Aliases before Pkg.mli and Pkg.ml
- **Must** create aliases node before library interface nodes
- **Must** include all child modules (both files and directories)
- **Must** enable library interface to depend on aliases

### REQ-057: Library Interface and Implementation Nodes
- **Given** a library directory
- **Example**: Create both Pkg.mli and Pkg.ml nodes
- **Must** create both interface (.mli) and implementation (.ml) nodes
- **Must** register both in module registry
- **Must** set open_modules to full aliases list (inherited + current)
- **Must** mark as generated if file doesn't exist, concrete if it does

### REQ-058: Library Dependency Chain
- **Given** library with aliases, interface, and implementation
- **Example**: aliases → interface → aliases, impl → aliases, impl → interface
- **Must** create edge from interface to aliases
- **Must** create edge from implementation to aliases
- **Must** create edge from implementation to interface
- **Must** enforce build order: aliases first, then interface, then implementation

### REQ-059: Context Propagation to Subdirectories
- **Given** processing enters a subdirectory
- **Example**: Enter net/ with context ns=["Std"], aliases=[Std__Aliases]
- **Must** extend namespace by appending directory module name
- **Must** append new aliases node to aliases list
- **Must** update parent_impl and parent_intf to library nodes
- **Must** pass updated context to children

## Dependency Wiring (ocamldep Integration)

### REQ-060: Dependency Extraction from Concrete Files
- **Given** a concrete .ml or .mli file
- **Example**: ocamldep foo.ml → ["String", "List", "Http_client"]
- **Must** run dependency analyzer (ocamldep) to extract module dependencies
- **Must** return list of module names

### REQ-061: Skip Dependency Extraction for Generated Files
- **Given** a generated file node
- **Example**: Aliases.ml.gen doesn't exist yet, skip ocamldep
- **When** file is marked as Generated
- **Must** skip dependency extraction (file doesn't exist on disk yet)
- **Must** assume no external dependencies
- **Must** rely only on explicit graph edges

### REQ-062: Local Module Dependency Resolution
- **Given** a module dependency name from ocamldep
- **Example**: Dependency "Client" found in registry → add edge
- **Must** attempt to find dependency in local module registry first
- **When** found, add edges to matching node(s)
- **Must** handle 0, 1, or 2 matching nodes (interface and/or implementation)

### REQ-063: Interface Cannot Depend on Implementation
- **Given** a .mli file with dependencies
- **Example**: foo.mli depends on Bar → only edge to Bar.mli, not Bar.ml
- **When** source is interface file (MLI kind)
- **When** target has both interface and implementation
- **Must** depend only on target's interface node
- **Must not** add edge from interface to any implementation
- **Must** prevent implementation details leaking into interfaces

### REQ-064: Implementation Depends on Available Nodes
- **Given** a .ml file with dependencies
- **Example**: foo.ml depends on Bar → edges to both Bar.mli and Bar.ml if both exist
- **When** source is implementation file (ML kind)
- **Must** add edges to all matching nodes (interface and/or implementation)
- **Must** allow depending on both .mli and .ml

### REQ-065: Silent External Dependency Resolution
- **Given** a module dependency not in local registry
- **Example**: Dependency "Kernel" not found → check build_results, then stdlib, then skip
- **Must** check if provided by build results (other packages)
- **Must** check if standard library module
- **Must** silently skip with no error (assume available)
- **Must not** add edges for external dependencies

## Cross-Package Dependencies

### REQ-066: Package Dependency List Excludes Self
- **Given** a package being built
- **Example**: Building "Std" with build_results ["Kernel", "Std"] → return ["Kernel"]
- **Must** return all registered packages from build results
- **Must** exclude current package name
- **Must** preserve registration order

### REQ-067: Transitive Dependency Accumulation
- **Given** package A depends on package B, B depends on C
- **Example**: Build order [C, B, A], each accumulates outputs
- **Must** make earlier packages' artifacts available to later packages
- **Must** copy all registered outputs to sandbox for each build
- **Must** enable transitive dependencies via build results accumulation

## Build Order Computation

### REQ-068: Topological Sort for Build Order
- **Given** a populated dependency graph
- **Example**: Graph with edges → [node1, node3, node2, ...] in valid build order
- **Must** compute topological sort of all nodes
- **Must** ensure all dependencies built before dependents
- **Must** produce deterministic order for same graph

### REQ-069: Cycle Detection and Reporting
- **Given** a dependency graph with cycles
- **Example**: A → B → C → A forms cycle
- **When** cycle detected during topological sort
- **Must** report all nodes in cycle
- **Must** include node IDs and file paths for debugging
- **Must** mark generated files with "(generated)" in output
- **Must** fail the build with clear error message
- **Must** re-raise Graph.Cycle exception

### REQ-070: Iteration in Build Order Skips Root
- **Given** a callback function to apply to nodes
- **Example**: iter(callback) applies to all except root node
- **Must** apply function to nodes in topological order
- **Must** skip root node (kind=Root)
- **Must** only process real modules (ML, MLI, C, H)

## Graph Visualization

### REQ-071: DOT Format Export with Package Name
- **Given** a dependency graph
- **Example**: Export to Graphviz with graph name "Kernel"
- **Must** support exporting graph to DOT format
- **Must** use package name as graph name

### REQ-072: Node Labels with Generation Markers
- **Given** graph nodes for visualization
- **Example**: Concrete "foo.ml" → label "foo.ml", Generated → label "foo.ml (gen)"
- **Must** use basename for concrete files
- **Must** append " (gen)" suffix for generated files

### REQ-073: Node Colors by Kind
- **Given** graph nodes to visualize
- **Example**: Interfaces blue, implementations green, C files yellow
- **Must** color interface files blue (lightblue fill)
- **Must** color implementation files green (lightgreen fill)
- **Must** color C files red (lightyellow fill)
- **Must** leave other kinds unstyled

## Public API and Operations

### REQ-074: Graph Creation and Initialization
- **Given** root directory, package name, and build results
- **Example**: make(root="/pkg", package_name="kernel", build_results=registry)
- **Must** initialize src_root as {root}/src
- **Must** walk file tree from src_root
- **Must** create empty graph and registries
- **Must** normalize package name via Module_name.of_string
- **Must** return initialized dependency graph structure

### REQ-075: Full Graph Scanning Operation
- **Given** an initialized graph
- **Example**: scan() performs full analysis and returns populated graph
- **Must** perform full file tree scan starting from root
- **Must** construct complete dependency graph
- **Must** wire up all dependencies via ocamldep
- **Must** return fully populated graph ready for traversal

### REQ-076: Graph Iteration with Callback
- **Given** a populated graph and callback function
- **Example**: iter(fn) applies fn to each node in build order
- **Must** apply callback to each node in build order
- **Must** skip root node
- **Must** handle cycles by reporting and failing

### REQ-077: Registry Inspection for Debugging
- **Given** a dependency graph
- **Example**: print_registry() shows all registered modules sorted by name
- **Must** support printing registry contents
- **Must** sort output by module name for readability
- **Must** show node ID, module name, namespaced name, and file path

### REQ-078: Package Dependency Extraction
- **Given** a built package
- **Example**: get_dependencies() → ["Kernel", "Miniriot"] (packages built before)
- **Must** return list of package dependencies
- **Must** exclude self from list
- **Must** preserve build order

## Flat Compilation Model

### REQ-079: Flat Sandbox Directory Structure
- **Given** a build sandbox for compilation
- **Example**: All .cmi/.cmo files in sandbox/ root, not nested directories
- **Must** flatten all artifacts into sandbox root
- **Must** use basename only when copying files
- **Must** enable simple -I sandbox include path
- **Must** avoid nested -I paths

### REQ-080: Namespaced Artifacts Prevent Collisions
- **Given** modules from different namespaces
- **Example**: Std__Net__Client.cmi and Std__Data__Client.cmi coexist
- **Must** use namespaced names for all artifacts
- **Must** prevent name collisions in flat directory
- **Must** ensure unique filenames via namespace prefixes

## Both Interface and Implementation Rules

### REQ-081: Both Library Files Open Aliases
- **Given** library interface and implementation nodes
- **Example**: Both Pkg.mli and Pkg.ml have open_modules=[Pkg__Aliases, ...]
- **Must** add aliases to open_modules for both interface and implementation
- **Must** enable both to access child modules via short names
- **Must** invoke compiler with -open flags for both

### REQ-082: Implementation Dependencies
- **Given** library implementation node
- **Example**: Pkg.ml → Pkg__Aliases, Pkg.ml → Pkg.mli
- **Must** depend on aliases node
- **Must** depend on interface node
- **Must** enforce: aliases built first, then interface, then implementation

## Error Handling and Edge Cases

### REQ-083: Root Must Be Directory
- **Given** scan_from_root operation
- **Example**: If src/ is a file, fail with clear message
- **When** root of file tree is a file, not directory
- **Must** fail with error message including file path
- **Must not** attempt to process file as directory

### REQ-084: Deterministic Output Despite Hash Tables
- **Given** operations that produce user-visible output
- **Example**: Alias module contents sorted alphabetically
- **Must** produce identical results for identical input
- **Must** sort all lists used in output generation
- **Must not** rely on hash table iteration order for user-visible behavior
- **Must** ensure reproducible builds

### REQ-085: Performance for Large Codebases
- **Given** a package with 1000+ modules
- **Example**: Efficient hash table lookups, avoid O(n²) operations
- **Must** handle packages with 1000+ modules efficiently
- **Must** avoid redundant filesystem operations
- **Must** avoid redundant dependency analysis
- **Must** use appropriate data structures (hash tables for lookups)

### REQ-086: Extension Validation for OCaml Files
- **Given** a file path being classified as OCaml module
- **Example**: Module.kind("foo.txt") → fail, Module.kind("foo.ml") → ok
- **When** file has extension other than .ml or .mli
- **Must** fail with clear error message
- **Must** include file path in error

### REQ-087: Library Interface Files Must Match Module Name
- **Given** a directory and potential library interface file
- **Example**: Directory "net" with "network.ml" → don't treat as library file
- **Must** compare module names, not file paths
- **Must** use case-insensitive module name matching
- **Must** handle OCaml capitalization rules correctly

## Debugging and Observability

### REQ-088: Logging for File Skipping
- **Given** a file with unrecognized extension
- **Example**: "Skipping file with ext=.txt: src/readme.txt"
- **Must** log message when skipping files
- **Must** include extension and file path
- **Must** not fail the build for unrecognized files

### REQ-089: Logging for Dependency Copying
- **Given** copying dependencies to sandbox
- **Example**: "  Copied dependency: kernel.cmi"
- **Must** print message for each file copied
- **Must** use format "  Copied dependency: {basename}"
- **Must** help user understand which packages are dependencies

### REQ-090: Package Scanning Announcement
- **Given** starting to scan a package
- **Example**: "Scanning package 'kernel' from /path/to/kernel"
- **Must** print message when starting package scan
- **Must** include package name and root directory
- **Must** help user track build progress

### REQ-091: Module Name Identity Conversion

* **Given** a normalized module name
* **Example**: `Module_name.to_string "Http_client"` → `"Http_client"`
* **Must** return the stored string unchanged (identity function)

### REQ-092: Disallow Duplicate Concrete Nodes

* **Given** attempts to add multiple nodes for the same concrete file path
* **Example**: two nodes targeting `"src/foo.ml"`
* **Must** ensure a single graph node per concrete file path
* **Must** reject or coalesce duplicates deterministically

### REQ-093: Header Files Copied to Sandbox (In-Package)

* **Given** `.h` files in the package
* **Example**: `sha256.h` alongside `sha256.c`
* **Must** copy header files into the build sandbox for compilation
* **Must** not introduce dependency edges for headers
* **Must** keep headers available to C compilation steps

### REQ-094: Namespace Policy Checker API

* **Given** two namespaced modules
* **Example**: `is_dependency_allowed from=Std__Net__Http__Client to=Std__Net`
* **Must** expose a pure function that implements REQ-035..039 rules
* **Must** return boolean without side effects
* **Must** be usable for validation and diagnostics even if not enforced automatically

### REQ-095: Deterministic Registry Printing

* **Given** a request to print the module registry
* **Example**: `print_registry()` output
* **Must** sort entries lexicographically by module name
* **Must** display node ID, module name, namespaced name, and file path
* **Must** include both interface and implementation entries when present

### REQ-096: Use User-Supplied Library Files Selectively

* **Given** a library where only one of `{Lib}.ml` or `{Lib}.mli` exists
* **Example**: `Lib.mli` exists, `Lib.ml` missing
* **Must** use the existing file as Concrete
* **Must** generate only the missing counterpart
* **Must** still wire edges per REQ-058 (impl → intf and aliases)

### REQ-097: Alias Generation Uses Implementation Set

* **Given** a library with modules that have both `.mli` and `.ml`
* **Example**: `Foo.mli` + `Foo.ml`
* **Must** compute alias targets from the set of implementation modules only
* **Must** ensure exactly one alias per logical module name

### REQ-098: Graph Node Kind “Other”

* **Given** internal nodes that are not `.ml/.mli/.c/.h/Root`
* **Example**: future artifact or tool marker node
* **Must** allow `kind=Other "<label>"`
* **Must** skip from compilation ordering unless explicitly wired by edges

### REQ-099: DOT Export – Generated Marker Suffix

* **Given** DOT node labels for generated files
* **Example**: label `"Aliases.ml (gen)"` for generated aliases
* **Must** append `" (gen)"` exactly to distinguish from concrete files
* **Must** keep concrete file labels as basenames only

### REQ-100: Package Scan Start/End Logs

* **Given** a package scan operation
* **Example**: scanning `"kernel"` at `/path/to/kernel`
* **Must** log a start message with package name and root
* **Should** log a completion message with basic stats (files, modules, duration)

### REQ-101: Build Results – Lookup by Module Name Only

* **Given** queries into cross-package build results
* **Example**: `has_module "Kernel"`
* **Must** perform lookups by normalized `Module_name` only
* **Must not** rely on raw package directory strings

### REQ-102: Topological Iterator API Contract

* **Given** an iterator over build nodes
* **Example**: `iter (fun node -> ...)`
* **Must** traverse in strict topological order
* **Must** skip the Root node
* **Must** re-raise cycle errors encountered during ordering (see REQ-069)

### REQ-103: ocamldep Resolution Preference

* **Given** dependency names returned by ocamldep
* **Example**: dependency `"Client"`
* **Must** normalize names via module-name normalization (REQ-001)
* **Must** prefer local registry matches over external packages
* **Must** fall back to external/stdlib per REQ-065

### REQ-104: Library Parent Edge Direction

* **Given** edges between library parent and children
* **Example**: `Parent.mli → Child.mli`, `Parent.ml → Child.ml`
* **Must** direct edges from parent to child (parent depends on child’s availability)
* **Must** maintain separate edges by kind (MLI-to-MLI, ML-to-ML)

### REQ-105: Consistent Sorting Inputs for Generation

* **Given** any code generation step (aliases, library stubs)
* **Example**: alias entries, `module X = ...` lines
* **Must** sort input sets before emission
* **Must** ensure stable output across runs with identical inputs
