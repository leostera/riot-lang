(** Shell completion generation library *)
type shell =
  Zsh
  | Bash
  | Fish

(** Convert shell enum to string *)
val shell_to_string: shell -> string

(** Parse shell from string *)
val shell_from_string: string -> shell option

(** Generate completion script for a shell *)
val generate_script: shell -> string

(** List all packages in workspace *)
val list_packages: Riot_model.Workspace.t -> string list

(** List all binaries in workspace as "binary:package" for display *)
val list_binaries: Riot_model.Workspace.t -> string list

(** List all test binaries in workspace as "test:package" for display *)
val list_tests: Riot_model.Workspace.t -> string list

(** List test binaries as "package:test" for display in completions.
    Also includes "package:..." for running all tests in a package. *)
val list_benchmarks: Riot_model.Workspace.t -> string list

(** List benchmark binaries as "package:bench" for display in completions.
    Also includes "package:..." for running all benchmarks in a package. *)
val list_commands: Riot_model.Workspace.t -> string list

(** List all package command descriptions matching the order of list_commands *)
val list_command_descriptions: Riot_model.Workspace.t -> string list
