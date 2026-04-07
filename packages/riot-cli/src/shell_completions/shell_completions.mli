(** Shell-completion script generation for Riot.

    Use this module to build shell-specific completion scripts and the
    completion candidates that depend on the current workspace.
*)
type shell =
  | Zsh
  | Bash
  | Fish

(** Render the shell tag used by completion generation. *)
val shell_to_string: shell -> string

(** Parse a shell name such as ["zsh"], ["bash"], or ["fish"]. *)
val shell_from_string: string -> shell option

(** Generate the completion script for a shell. *)
val generate_script: shell -> string

(** List package names available in the workspace for completion. *)
val list_packages: Riot_model.Workspace.t -> string list

(** List runnable binaries as completion labels.

    Example return values look like ["serve:my-package"].
*)
val list_binaries: Riot_model.Workspace.t -> string list

(** List test-suite selectors for completion.

    Example return values look like ["math-tests:std"].
*)
val list_tests: Riot_model.Workspace.t -> string list

(** List benchmark selectors for completion.

    The result also includes package-wide selectors such as ["std:..."] for
    running all benchmarks in a package.
*)
val list_benchmarks: Riot_model.Workspace.t -> string list

(** List command labels exposed by workspace packages. *)
val list_commands: Riot_model.Workspace.t -> string list

(** List command descriptions matching the order of {!list_commands}. *)
val list_command_descriptions: Riot_model.Workspace.t -> string list
