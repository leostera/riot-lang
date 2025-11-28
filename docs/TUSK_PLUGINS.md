# Design and implement plugin system for Tusk

## Description

Create an extensible plugin architecture that allows third-party developers to extend Tusk's functionality by accessing build graphs, type information, and hooking into the build pipeline.

## Architecture Overview

### 1\. Core Data Access (Read-Only)

Expose Tusk's internal data structures through a stable API:

* **Module graph** - dependency relationships
* **Build graph** - compilation actions and dependencies
* **Type information** - from `.cmt`/`.cmti` files
* **Store/cache** - build artifacts
* **AST** - parsed source code

### 2\. Plugin Hooks/Extension Points

```ocaml
type plugin_hook =
  | BeforeBuild of Build_graph.t
  | AfterBuild of Build_results.t
  | BeforeFormat of Path.t list
  | AfterFormat of Path.t list
  | AfterTypeCheck of (Path.t * Types.signature) list
  | OnModuleGraphBuilt of Module_graph.t
  | OnCacheHit of Artifact.t
  | OnFileChange of Path.t
  | OnServerStart
  | OnServerStop
```

### 3\. Plugin Communication Models

#### Option A: RPC-Based (Recommended)

* Plugins run as separate processes
* Communicate via JSON-RPC over stdio/sockets
* Natural sandboxing via process isolation
* Language-agnostic (plugins in any language)

#### Option B: WebAssembly

* Compile plugins to WASM
* Run in sandboxed WASM runtime
* Platform independent
* Safe by default

#### Option C: Dynamic Loading

* Load `.cmxs` files (OCaml native dynlink)
* Direct API access (fastest)
* Same-language only
* Requires careful API versioning

### 4\. Plugin API

```ocaml
module Plugin_api : sig
  module Query : sig
    val get_module_graph : unit -> Module_graph.t
    val get_build_graph : unit -> Build_graph.t
    val get_package_info : string -> Package.t option
    val query_types : Path.t -> Types.signature option
    val find_definition : symbol:string -> Location.t option
    val find_references : symbol:string -> Location.t list
  end
  
  module Actions : sig
    val add_build_action : Actions.action -> unit
    val add_mcp_tool : name:string -> handler -> unit
    val register_formatter : extension:string -> formatter -> unit
    val add_cli_command : Command.t -> unit
  end
  
  module Events : sig
    type event =
      | BuildStarted of { packages : string list }
      | BuildCompleted of { results : Build_results.t }
      | FileChanged of { path : Path.t }
      | TypeCheckCompleted of { path : Path.t; signature : Types.signature }
    
    val subscribe : event_type -> (event -> unit) -> subscription
    val unsubscribe : subscription -> unit
  end
  
  module Storage : sig
    val get : string -> string option
    val set : string -> string -> unit
    val delete : string -> unit
  end
end
```

### 5\. Plugin Manifest

```toml
# plugin.toml
[plugin]
name = "tusk-security-scanner"
version = "1.0.0"
author = "security-team"
description = "Scan dependencies for vulnerabilities"

[plugin.runtime]
type = "rpc"  # or "wasm" or "native"
command = "tusk-security-scanner"

[plugin.hooks]
after_build = true
on_module_graph_built = true

[plugin.permissions]
read_module_graph = true
read_types = true
network = true  # for vulnerability database
```

### 6\. Plugin Discovery & Installation

```bash
# Install from registry
tusk plugin install security-scanner

# Install from local path
tusk plugin install ./my-plugin

# List installed plugins
tusk plugin list

# Remove plugin
tusk plugin remove security-scanner
```

### 7\. Example Plugin Implementations

#### Linter Plugin

```ocaml
(* my_linter_plugin.ml *)
let analyze_module path signature =
  (* Access AST and types *)
  let issues = find_code_smells signature in
  List.iter (fun issue ->
    Plugin_api.Report.warning ~file:path ~line:issue.line issue.message
  ) issues

let () =
  Plugin_api.Events.subscribe TypeCheckCompleted (function
    | TypeCheckCompleted { path; signature } ->
        analyze_module path signature
    | _ -> ()
  )
```

#### Code Generator Plugin

```ocaml
let generate_serializers module_graph =
  Module_graph.iter (fun module_info ->
    match find_record_types module_info with
    | [] -> ()
    | types ->
        let code = generate_json_serializers types in
        Plugin_api.Actions.add_build_action 
          (WriteFile { path = "serializers.ml"; content = code })
  ) module_graph
```

#### Custom MCP Tool

```ocaml
let () =
  Plugin_api.Actions.add_mcp_tool 
    ~name:"analyze-complexity"
    ~handler:(fun args ->
      let graph = Plugin_api.Query.get_module_graph () in
      let complexity = calculate_cyclomatic_complexity graph in
      `Assoc ["complexity", `Int complexity]
    )
```

## Security Considerations

### Sandboxing

* Plugins run in separate processes/WASM
* No direct filesystem access (go through API)
* Capability-based permissions
* Resource limits (CPU, memory, timeout)

### API Stability

* Versioned plugin API
* Backward compatibility guarantees
* Deprecation warnings
* Plugin compatibility checking

## Implementation Phases

### Phase 1: Core API

* Define Plugin_api module
* Implement query functions
* Add event system

### Phase 2: RPC Communication

* JSON-RPC protocol
* Process management
* Plugin lifecycle

### Phase 3: Plugin Management

* Installation/removal
* Discovery/registry
* Configuration

### Phase 4: Example Plugins

* Create reference plugins
* Documentation
* Plugin development kit

## Benefits

* **Extensibility** - Third-party tools and integrations
* **Innovation** - Community can experiment freely
* **Modularity** - Core stays lean, features in plugins
* **Safety** - Sandboxed execution
* **Language agnostic** - Plugins in any language (with RPC)

## Use Cases

* Custom linters and analyzers
* Code generators (serialization, boilerplate)
* Build reporters and metrics
* Security scanners
* Documentation generators
* Custom MCP tools
* IDE integrations
* CI/CD integrations

## Metadata
- URL: [https://linear.app/andes-sh/issue/RIOT-44/design-and-implement-plugin-system-for-tusk](https://linear.app/andes-sh/issue/RIOT-44/design-and-implement-plugin-system-for-tusk)
- Identifier: RIOT-44
- Status: Backlog
- Priority: No priority
- Assignee: Unassigned
- Labels: Feature
- Created: 2025-10-05T22:29:39.504Z
- Updated: 2025-10-05T22:29:53.572Z
