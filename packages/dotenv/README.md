# dotenv

`dotenv` reads dotenv-style configuration and can either apply it to the current
process environment or parse it without mutating anything.

Use it when an application wants local configuration from `.env`, test or dev
overrides from `.env.test` / `.env.dev` / `.env.local`, or tooling that needs to
inspect dotenv files safely.

## Which Function Should I Use?

- Use `Dotenv.load ()` when `.env` is required for the application to start. A
  missing file returns a `ReadError`.

- Use `Dotenv.load_if_exists ()` when `.env` is optional. This is the right
  default for local developer overrides where the application should keep
  booting if no dotenv file is present.

- Use `Dotenv.load ~env:"test" ()` when the application should load a profile
  overlay such as `.env.test` before `.env`.

- Use `Dotenv.load_files [...]` when the application already has an explicit
  list of dotenv files. Put the highest-priority file first.

- Use `Dotenv.parse source` when you want to validate or inspect dotenv content
  without touching the process environment.

- Use `Dotenv.parse_files [...]` when you want to read dotenv files for
  validation, previews, editor tooling, or tests without applying the values.

## Load `.env`

`Dotenv.load ()` reads `.env`, parses it, and applies the parsed bindings to the
process environment. Existing process environment variables are preserved by
default.

```ocaml
match Dotenv.load () with
| Ok applied ->
    Std.println ("applied " ^ Std.Int.to_string (Std.List.length applied) ^ " variables")
| Error error ->
    Std.eprintln (Dotenv.error_to_string error)
```

If the file is optional, use `load_if_exists` instead:

```ocaml
match Dotenv.load_if_exists () with
| Ok [] ->
    Std.println "no dotenv file loaded"
| Ok applied ->
    Std.println ("applied " ^ Std.Int.to_string (Std.List.length applied) ^ " variables")
| Error error ->
    Std.eprintln (Dotenv.error_to_string error)
```

## Load `.env.{ENV}`

Profile loading gives the profile file higher precedence than the base file.
For `~env:"test"`, the candidate paths are `.env.test` and `.env`.

```ocaml
match Dotenv.load ~env:"test" () with
| Ok _applied -> ()
| Error error ->
    Std.eprintln (Dotenv.error_to_string error)
```

You can inspect the derived paths directly:

```ocaml
Dotenv.env_paths ~env:"test" ()
(* [Std.Path.v ".env.test"; Std.Path.v ".env"] *)
```

Custom base paths work the same way:

```ocaml
Dotenv.env_paths ~path:(Std.Path.v "config/.env") ~env:"local" ()
(* [Std.Path.v "config/.env.local"; Std.Path.v "config/.env"] *)
```

## Existing Environment Values

The default policy is `Dotenv.PreserveExisting`. If the process already has a
value for a key, the dotenv value is skipped and does not appear in the returned
`applied` list.

Use `Dotenv.OverwriteExisting` when the dotenv file should replace process
environment values:

```ocaml
Dotenv.load ~on_existing:Dotenv.OverwriteExisting ()
```

The policy is explicit on purpose. It makes call sites show whether dotenv is a
fallback source of local defaults or the source of truth for the process.

## Missing Files

`Dotenv.load ()` requires at least one candidate file. `Dotenv.load_if_exists ()`
skips missing candidates and returns `Ok []` when none exist.

For explicit file lists, missing files are skipped by default:

```ocaml
Dotenv.load_files
  [ Std.Path.v ".env.local"; Std.Path.v ".env" ]
```

Use `~on_missing:Dotenv.FailMissing` when every path in the list must exist:

```ocaml
Dotenv.load_files
  ~on_missing:Dotenv.FailMissing
  [ Std.Path.v ".env" ]
```

## Parse Without Applying

Parsing returns bindings with the key, parsed value, and 1-based source line.
It does not mutate the process environment.

```ocaml
match Dotenv.parse "HOST=localhost\nURL=http://${HOST}:8080\n" with
| Ok bindings ->
    Std.List.for_each bindings ~fn:(fun binding ->
      Std.println
        (binding.key ^ "=" ^ binding.value ^ " from line " ^ Std.Int.to_string binding.line))
| Error error ->
    Std.eprintln (Dotenv.error_to_string error)
```

For files, use `parse_files`:

```ocaml
match Dotenv.parse_files [ Std.Path.v ".env"; Std.Path.v ".env.local" ] with
| Ok bindings ->
    Std.println ("parsed " ^ Std.Int.to_string (Std.List.length bindings) ^ " bindings")
| Error error ->
    Std.eprintln (Dotenv.error_to_string error)
```

## Supported Syntax

- `KEY=value` assignments are supported, including spaces around the separator.

- `KEY: value` assignments are supported for compatibility with dotenv files
  that use YAML-style separators.

- `export KEY=value` is accepted, so files copied from shell-oriented examples
  do not need to be rewritten.

- `export KEY` reuses a value already present earlier in the file or in the
  process environment.

- Single-quoted values are literal. Variable references inside single quotes are
  not substituted.

- Double-quoted values support escapes, multi-line content, and variable
  substitution.

- Unquoted values support inline comments after whitespace and variable
  substitution.

- `$KEY` and `${KEY}` substitutions are supported in unquoted and double-quoted
  values.

- Process environment values take precedence during substitution. Values parsed
  earlier in the dotenv source are used only when the process environment does
  not already define the referenced key.

- UTF-8 BOMs are stripped from the beginning of files.

- LF, CRLF, and CR line endings are normalized.

- Shell command substitution such as `$(command)` is intentionally not
  supported.

## Telemetry

`dotenv` emits `Std.Telemetry.event` constructors under `Dotenv.Events` for
parse and load activity. Use these when you want to observe dotenv behavior
without wrapping every call site.

```ocaml
ignore (Std.Telemetry.start ());

Std.Telemetry.attach
  "dotenv-observer"
  (fun event ->
    match event with
    | Dotenv.Events.Loaded { path; binding_count } ->
        Std.println
          ("loaded "
           ^ Std.Path.to_string path
           ^ " with "
           ^ Std.Int.to_string binding_count
           ^ " bindings")
    | Dotenv.Events.LoadSkipped { path } ->
        Std.println ("skipped missing " ^ Std.Path.to_string path)
    | _ -> ())
```

## Errors

All parsing and loading functions return `('a, Dotenv.error) Std.Result.t`.
Use pattern matching when the application needs structured handling, or
`Dotenv.error_to_string` when a stable human-readable message is enough.

```ocaml
match Dotenv.load () with
| Ok _ -> ()
| Error (Dotenv.ReadError { path; reason }) ->
    Std.eprintln ("could not read " ^ Std.Path.to_string path ^ ": " ^ reason)
| Error (Dotenv.ParseError { line; message }) ->
    Std.eprintln ("invalid dotenv on line " ^ Std.Int.to_string line ^ ": " ^ message)
```

## Runnable Examples

```sh
riot run -p dotenv parse_string
riot run -p dotenv load_if_exists
riot run -p dotenv profile_loading
```

## Development Checks

```sh
riot test -p dotenv
riot bench -p dotenv
riot doc -p dotenv
```

See [CHANGELOG.md](CHANGELOG.md) for migration notes and the detailed behavior
diff from the earlier implementation.
