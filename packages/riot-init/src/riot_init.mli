open Std

type package_kind =
  | Library
  | Binary

type event =
  | WorkspaceInitializationStarted of { name: string; target_dir: Path.t }
  | ScaffoldCreated of { path: string }
  | WorkspaceInitializationCompleted of {
      next_steps: string list;
      package_hints: (package_kind * string) list;
    }

(** ArgParser command definition for `riot init`. *)
val command: Std.ArgParser.command

(** Execute `riot init` with parsed arguments. *)
val run: on_event:(event -> unit) -> Std.ArgParser.matches -> (unit, exn) result
