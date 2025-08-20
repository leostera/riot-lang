# Tusk Plugin System Design

## Overview

A comprehensive plugin system that makes Tusk extensible through workspace-local commands, dependency-provided commands, and custom scripts. This enables the OCaml ecosystem to build rich tooling that integrates seamlessly with the build system.

## Philosophy

### Core Principles
1. **Zero Installation Friction**: Commands from dependencies work immediately after `tusk build`
2. **Type-Safe Integration**: All plugins use OCaml interfaces, not shell scripts
3. **RPC-First**: Plugins get full access to Tusk's RPC protocol and workspace state
4. **Workspace-Aware**: Commands understand the project structure and dependencies
5. **Composable**: Commands can call other commands, build complex workflows

### Use Cases
- **Web frameworks** provide scaffolding: `tusk dream scaffold --api users`
- **Testing libraries** provide runners: `tusk alcotest run --watch`
- **Database tools** provide migrations: `tusk db migrate --version 42`
- **Deployment tools** provide workflows: `tusk deploy staging --env production`
- **Custom workflows** via scripts: `tusk setup` (clean + build + migrate + test)

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Tusk Core                                │
├─────────────────────────────────────────────────────────────────┤
│  Command Registry                                               │
│  ├── Built-in commands (build, run, clean, test)               │
│  ├── Workspace commands (from workspace.toml)                  │
│  ├── Dependency commands (from dependencies)                   │
│  └── Script aliases (from workspace.toml [scripts])            │
├─────────────────────────────────────────────────────────────────┤
│  Plugin Runtime                                                │
│  ├── Dynamic loading (.cmxs files)                             │
│  ├── RPC client library                                        │
│  ├── Command interface                                         │
│  └── Workspace context                                         │
├─────────────────────────────────────────────────────────────────┤
│  RPC Server (packages/tusk-rpc)                                │
│  ├── Build operations                                          │
│  ├── Workspace queries                                         │
│  ├── File operations                                           │
│  └── Process management                                        │
└─────────────────────────────────────────────────────────────────┘
```

## Plugin Types

### 1. Workspace Commands

Commands defined directly in the workspace for project-specific tooling.

#### Configuration in `workspace.toml`
```toml
[extensions]
commands = [
  { name = "deploy", module = "Tools.Deploy_command" },
  { name = "db", module = "Tools.Database_command" },
  { name = "lint", module = "Tools.Lint_command" }
]
```

#### Implementation Example
```ocaml
(* tools/deploy_command.ml *)
open Tusk_lib

module Deploy_command : Tusk_command.S = struct
  let name = "deploy"
  let description = "Deploy application to various environments"
  
  let arguments = [
    Arg.string "environment" ~doc:"Target environment (staging, production)";
    Arg.flag "dry-run" ~doc:"Show what would be deployed without doing it";
    Arg.option "version" ~doc:"Specific version to deploy";
  ]
  
  let run workspace args =
    let open Tusk_rpc in
    let* client = Client.connect () in
    
    match Args.get_string args "environment" with
    | "staging" ->
        let* () = Client.build_package client "api" in
        let* () = deploy_to_staging workspace args in
        Ok 0
    | "production" ->
        let* () = Client.build_all client in
        let* () = deploy_to_production workspace args in
        Ok 0
    | env ->
        Error (Printf.sprintf "Unknown environment: %s" env)
end
```

### 2. Dependency Commands

Commands provided by installed dependencies that register themselves.

#### Configuration in Dependency's `package.toml`
```toml
[package]
name = "dream"
version = "1.0.0"

[[command]]
name = "dream"
description = "Dream web framework scaffolding and tools"
module = "Dream_tusk.Command"
```

#### Implementation in Dependency
```ocaml
(* dream_tusk/command.ml - in the Dream package *)
open Tusk_lib

module Command : Tusk_command.S = struct
  let name = "dream"
  let description = "Dream web framework scaffolding and development tools"
  
  let arguments = [
    Arg.subcommand [
      "scaffold", [
        Arg.string "type" ~doc:"Type of component (route, handler, middleware)";
        Arg.string "name" ~doc:"Name of the component";
      ];
      "routes", [
        Arg.flag "list" ~doc:"List all registered routes";
      ];
      "serve", [
        Arg.int "port" ~default:8080 ~doc:"Port to serve on";
        Arg.flag "watch" ~doc:"Watch for changes and reload";
      ];
    ]
  ]
  
  let run workspace args =
    let open Tusk_rpc in
    let* client = Client.connect () in
    
    match Args.get_subcommand args with
    | "scaffold" ->
        let type_ = Args.get_string args "type" in
        let name = Args.get_string args "name" in
        scaffold_component workspace ~type_ ~name
    | "routes" ->
        if Args.get_flag args "list" then
          list_routes workspace
        else
          show_routes_help ()
    | "serve" ->
        let port = Args.get_int args "port" in
        let watch = Args.get_flag args "watch" in
        serve_application workspace ~port ~watch
    | cmd ->
        Error (Printf.sprintf "Unknown dream subcommand: %s" cmd)
end
```

### 3. Script Aliases

Simple command sequences defined in workspace configuration.

#### Configuration in `workspace.toml`
```toml
[scripts]
# Simple command sequences
setup = ["clean", "build", "db.migrate", "test"]
ci = ["lint", "test", "build --release"]
deploy-staging = ["test", "build --release", "deploy staging"]

# Scripts with arguments
test-watch = ["test --watch $@"]  # Pass through all arguments
serve-dev = ["build", "dream serve --port 3000 --watch"]

# Conditional scripts (future enhancement)
[scripts.platform.linux]
install-deps = ["apt-get update", "apt-get install -y libssl-dev"]

[scripts.platform.macos]  
install-deps = ["brew install openssl"]
```

## Command Interface

### Core Interface (`packages/tusk-lib/command.mli`)
```ocaml
module type S = sig
  (** Command name (used in CLI) *)
  val name : string
  
  (** Short description for help text *)
  val description : string
  
  (** Long description with examples *)
  val help : string option
  
  (** Command line argument specification *)
  val arguments : Arg.spec list
  
  (** Main command implementation *)
  val run : Workspace.t -> Args.t -> (int, string) result
end

(** Argument parsing utilities *)
module Args : sig
  type t
  
  val get_string : t -> string -> string
  val get_int : t -> string -> int  
  val get_flag : t -> string -> bool
  val get_option : t -> string -> string option
  val get_list : t -> string -> string list
  val get_subcommand : t -> string
  val get_remaining : t -> string list
end

(** Workspace context *)
module Workspace : sig
  type t = {
    root : string;
    packages : Package.t list;
    config : Config.t;
    toolchain : Toolchain.t;
  }
  
  val find_package : t -> string -> Package.t option
  val get_package_path : t -> string -> string
  val get_target_dir : t -> string
end
```

### RPC Client Library (`packages/tusk-rpc/client.mli`)
```ocaml
(** RPC client for plugins to interact with Tusk server *)
module Client : sig
  type t
  
  val connect : unit -> (t, string) result
  val close : t -> unit
  
  (** Build operations *)
  val build_all : t -> (unit, string) result
  val build_package : t -> string -> (unit, string) result
  val clean : t -> (unit, string) result
  
  (** Workspace queries *)
  val get_workspace : t -> (Workspace.t, string) result
  val get_packages : t -> (Package.t list, string) result
  val get_dependencies : t -> string -> (string list, string) result
  
  (** File operations *)
  val watch_files : t -> string list -> (File_event.t -> unit) -> (unit, string) result
  val generate_file : t -> path:string -> content:string -> (unit, string) result
  
  (** Process management *)
  val spawn_process : t -> cmd:string -> args:string list -> (Process.t, string) result
  val run_command : t -> string -> (int * string * string, string) result
end
```

## Plugin Discovery and Loading

### Discovery Process
1. **Scan workspace.toml** for `[extensions]` section
2. **Scan dependencies** for `[[command]]` declarations in their package.toml
3. **Build plugin modules** as `.cmxs` dynamic libraries
4. **Register commands** in the command registry
5. **Load dynamically** when command is invoked

### Build Integration
```ocaml
(* During tusk build *)
let build_workspace_plugins workspace =
  let plugin_dir = Filename.concat workspace.root "target/debug/plugins" in
  System.mkdirp plugin_dir;
  
  (* Build workspace commands *)
  List.iter (fun cmd ->
    let* () = compile_plugin_module 
      ~source:(get_module_path workspace cmd.module)
      ~output:(Filename.concat plugin_dir (cmd.name ^ ".cmxs"))
      ~deps:[tusk_lib; tusk_rpc] in
    register_command cmd.name plugin_path
  ) workspace.config.extensions.commands;
  
  (* Build dependency commands *)
  List.iter (fun dep ->
    List.iter (fun cmd ->
      let* () = compile_dependency_plugin dep cmd in
      register_command cmd.name plugin_path
    ) dep.commands
  ) workspace.dependencies
```

### Dynamic Loading
```ocaml
(* Command execution *)
let execute_command name args =
  match Registry.lookup name with
  | Builtin cmd -> cmd.run args
  | Plugin plugin_path ->
      let module_name = load_plugin plugin_path in
      let module Cmd = (val module_name : Tusk_command.S) in
      Cmd.run workspace args
  | Script commands ->
      execute_script_sequence commands args
```

## Example Workflows

### Web Framework Integration
```bash
# Install Dream web framework
tusk add dream

# Dream automatically provides its command
tusk dream scaffold route --name users
# Creates: src/routes/users.ml with Dream route handlers

tusk dream scaffold middleware --name auth  
# Creates: src/middleware/auth.ml with Dream middleware

tusk dream serve --port 8080 --watch
# Builds and serves with hot reload
```

### Database Migration Tool
```bash
# Install a database library that provides migrations
tusk add postgresql-migrations

# Use the command it provides
tusk db create --name myapp_dev
tusk db migrate --version latest
tusk db rollback --steps 2
tusk db seed --file fixtures/users.sql
```

### Custom Workflow Scripts
```toml
# workspace.toml
[scripts]
# Development workflow
dev = [
  "clean",
  "db.migrate", 
  "build",
  "test --quick",
  "dream serve --watch"
]

# CI/CD workflow  
ci = [
  "lint",
  "test --coverage",
  "build --release",
  "deploy staging --dry-run"
]

# Release workflow
release = [
  "test --all",
  "build --release", 
  "tag-version",
  "deploy production",
  "notify-slack"
]
```

### Testing Framework Integration
```bash
# Install Alcotest
tusk add alcotest

# Alcotest provides its own command
tusk alcotest run                    # Run all tests
tusk alcotest run --watch            # Watch mode
tusk alcotest run --package mylib    # Test specific package
tusk alcotest run --filter user      # Filter by test name
```

## Security Considerations

### Sandboxing
- Plugins run in the same process but with limited capabilities
- File system access restricted to workspace directory
- Network access through RPC client only
- No direct system command execution

### Verification
- Plugin signatures verified against known dependencies
- Workspace plugins compiled from source (user controls them)
- Dependencies must be explicitly added to workspace

### Permission Model
```ocaml
module Permissions : sig
  type t = {
    file_read : string list;     (* Allowed read paths *)
    file_write : string list;    (* Allowed write paths *)
    network : bool;              (* Can make network requests *)
    spawn_process : bool;        (* Can spawn subprocesses *)
    rpc_access : string list;    (* Allowed RPC endpoints *)
  }
  
  val workspace_plugin : t       (* Full workspace access *)
  val dependency_plugin : t      (* Restricted access *)
end
```

## Implementation Phases

### Phase 1: Foundation (Week 1-2)
- Extract `packages/tusk-rpc` library
- Create `packages/tusk-lib` with command interface
- Implement basic plugin discovery and registration

### Phase 2: Workspace Commands (Week 3-4)
- Support for workspace.toml `[extensions]`
- Dynamic compilation of workspace plugins
- Command line argument parsing
- Basic RPC client integration

### Phase 3: Dependency Commands (Week 5-6)
- Package.toml `[[command]]` support
- Dependency plugin compilation during build
- Plugin loading and execution
- Security permissions framework

### Phase 4: Script Aliases (Week 7-8)
- Script configuration parsing
- Command sequence execution
- Argument passing and interpolation
- Error handling and rollback

### Phase 5: Advanced Features (Week 9-12)
- Plugin hot reloading during development
- Command completion generation
- Plugin marketplace/registry
- Performance optimizations

## Future Enhancements

### Plugin Marketplace
- Central registry of community plugins
- Plugin discovery: `tusk search database`
- Easy installation: `tusk plugin install dream-admin`
- Versioning and compatibility management

### IDE Integration
- Language server protocol for plugin development
- Auto-completion for plugin commands
- Debug support for plugin development
- Hot reloading during plugin development

### Advanced Scripting
- Conditional execution based on platform/environment
- Parallel script execution
- Script dependencies and ordering
- Template expansion in scripts

## Benefits

### For Users
- **Unified Interface**: Everything through `tusk` command
- **Zero Configuration**: Dependencies provide commands automatically
- **Type Safety**: All plugins are OCaml, compile-time checked
- **Performance**: Native compilation, no shell overhead

### For Library Authors
- **Easy Integration**: Simple interface to implement
- **Rich Context**: Full workspace and build system access
- **Powerful APIs**: RPC access to all Tusk functionality
- **Distribution**: Plugins distributed with libraries

### For Ecosystem
- **Consistent Tooling**: All OCaml tools follow same patterns
- **Composability**: Commands can be combined in scripts
- **Innovation**: Easy to experiment with new workflows
- **Growth**: Lower barrier to creating OCaml tooling

This plugin system would make Tusk the central hub for all OCaml development activities, similar to how `cargo` works for Rust but with much richer extensibility through the actor model and RPC system.