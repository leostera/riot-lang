(** Shell completion generation library *)

type shell = Zsh | Bash | Fish

(** Convert shell enum to string *)
val shell_to_string : shell -> string

(** Parse shell from string *)
val shell_from_string : string -> shell option

(** Generate completion script for a shell *)
val generate_script : shell -> string

(** List all packages in workspace *)
val list_packages : Tusk_model.Workspace.t -> string list

(** List all binaries in workspace as "binary:package" for display *)
val list_binaries : Tusk_model.Workspace.t -> string list

(** List all test binaries in workspace as "test:package" for display *)
val list_tests : Tusk_model.Workspace.t -> string list
