open Std

let not_in_workspace_message = "not in a riot workspace\n\n"
^ "Riot could not find a riot.toml in this directory or any parent directory.\n\n"
^ "To initialize a workspace here, run:\n\n"
^ "    riot init\n\n"
^ "To create a new package directory, run:\n\n"
^ "    riot new <name>"

let not_in_workspace_failure = "not in a riot workspace"

let print_not_in_workspace = fun () -> eprintln not_in_workspace_message
