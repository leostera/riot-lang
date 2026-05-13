# Riot Package Commands

Package-provided commands are workspace-local tools declared by a package and
invoked through the Riot CLI as `riot <package>:<command> [args...]`.

## Manifest

Declare commands in the package `riot.toml`:

```toml
[[command]]
name = "say"
help = "Say text from a package command"
path = "src/say_cmd.ml"
```

- `name` is the command name after the colon.
- `help` is shown by dynamic completion helpers.
- `path` points at the command source, relative to the package root.

## Invocation

Run a package command from anywhere inside the workspace:

```sh
riot toolbox:say hello from riot
```

Riot discovers the package command, builds the package command binary for the
host target when needed, then replaces the Riot process with the command binary.
Arguments after `<package>:<command>` are passed to the command binary.

Use completion data to inspect commands available in the current workspace:

```sh
riot completions --commands
```

## Command Source Pattern

Package commands are built as binaries. The source should run its `main` when
the compiled command binary is executed:

```ocaml
open Std

let name = "say"

let main ~args =
  let args =
    match args with
    | _program :: rest -> rest
    | [] -> []
  in
  println ("hello " ^ String.concat " " args);
  Ok ()

let should_autorun =
  match Env.args with
  | argv0 :: _ -> (
      match Path.from_string argv0 with
      | Ok path -> String.equal (Path.basename path) name
      | Error _ -> String.equal argv0 name)
  | [] -> false

let () =
  if should_autorun then
    let _ = Runtime.run ~main ~args:Env.args () in
    ()
```

Use `Std.ArgParser` inside `main` when the package command needs its own flags
or subcommands.
