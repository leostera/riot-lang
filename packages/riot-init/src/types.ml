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

let package_hints =
  [
    Library, "riot new --lib ./packages/<name>";
    Binary, "riot new --bin ./packages/<name>";
  ]

let emit = fun ~on_event event -> on_event event
