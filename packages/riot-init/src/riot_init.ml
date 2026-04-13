open Std
open Riot_model

let ( let* ) result fn = Result.and_then result ~fn

(** ArgParser command definition *)
let command =
  let open ArgParser in
    let open Arg in command "init"
    |> about "Initialize a new Riot workspace"
    |> args
      [
        positional "path" |> required false |> help "Path for new workspace (default: current directory)";
        flag "name" |> long "name" |> short 'n' |> help "Workspace name (default: directory basename)";
        flag "lib" |> long "lib" |> help "Create library package (default)";
        flag "bin" |> long "bin" |> help "Create binary package";
      ]

(** Validate package name using Riot_model *)
let validate_name = fun name ->
  match Package.validate_name name with
  | Ok n -> Ok n
  | Error msg -> Error (Failure msg)

(** Convert package name to module name using Riot_model *)
let package_name_to_module_name = fun name -> Module_name.of_string name |> Module_name.to_string

let module_name_to_test_file_stem = fun module_name -> String.lowercase_ascii module_name ^ "_tests"

type package_kind =
  | Library
  | Binary

type event =
  | WorkspaceInitializationStarted of { name: string; target_dir: Path.t }
  | ScaffoldCreated of { path: string }
  | WorkspaceInitializationCompleted of {
      next_steps: string list;
      package_hints: (package_kind * string) list
    }

let package_hints = [
  (Library, "riot new --lib ./packages/<name>");
  (Binary, "riot new --bin ./packages/<name>");
]

let next_steps = fun ~cwd ~target_dir ~path_arg ~is_library ~workspace_name ->
  let steps = ref [] in
  if Option.is_some path_arg && not (Path.equal cwd target_dir) then
    steps := !steps @ [ "cd " ^ Path.to_string target_dir ];
  steps := !steps @ [ "riot build"; "riot test" ];
  if not is_library then
    steps := !steps @ [ "riot run " ^ workspace_name ];
  !steps

let emit = fun ~on_event event -> on_event event

(** Create workspace riot.toml *)
let create_workspace_toml = fun ~on_event target_dir workspace_name ->
  let content = {|[workspace]
name = "|} ^ workspace_name ^ {|"
members = [
  "packages/|} ^ workspace_name ^ {|",
]

[dependencies]
# Shared external dependencies

[profile.debug]
kind = "native"
|}
  in
  let path = Path.(target_dir / Path.v "riot.toml") in
  match Fs.write content path with
  | Ok () ->
      emit ~on_event (ScaffoldCreated { path = "riot.toml" });
      Ok ()
  | Error _e -> Error (Failure "Failed to create riot.toml")

(** Create ocaml-toolchain.toml *)
let create_toolchain_toml = fun ~on_event target_dir ->
  let content = {|[toolchain]
version = "5.5.0-riot.2"
|}
  in
  let path = Path.(target_dir / Path.v "ocaml-toolchain.toml") in
  match Fs.write content path with
  | Ok () ->
      emit ~on_event (ScaffoldCreated { path = "ocaml-toolchain.toml" });
      Ok ()
  | Error _e -> Error (Failure "Failed to create ocaml-toolchain.toml")

(** Create .gitignore *)
let create_gitignore = fun ~on_event target_dir ->
  let content = {|# Riot build artifacts
_build
|}
  in
  let path = Path.(target_dir / Path.v ".gitignore") in
  match Fs.write content path with
  | Ok () ->
      emit ~on_event (ScaffoldCreated { path = ".gitignore" });
      Ok ()
  | Error _e -> Error (Failure "Failed to create .gitignore")

(** Create README.md *)
let create_readme = fun ~on_event target_dir workspace_name is_library ->
  let running_section =
    if is_library then
      ""
    else
      {|
### Running

Run the starter binary:
```bash
riot run |} ^ workspace_name ^ {|
```
|}
  in
  let container_section =
    if is_library then
      {|
### Containers

Verify the workspace in Docker:
```bash
docker build -t |} ^ workspace_name ^ {| .
```
|}
    else
      {|
### Containers

Build the starter application container:
```bash
docker build -t |} ^ workspace_name ^ {| .
```
|}
  in
  let content = {|# |} ^ workspace_name ^ {|

A Riot workspace for OCaml development.

## Getting Started

### Building

Build all packages:
```bash
riot build
```
|} ^ running_section ^ {|

### Testing

Run tests:
```bash
riot test
```
|} ^ container_section ^ {|

### Continuous Integration

The generated `.github/workflows/ci.yml` runs `riot build` and `riot test` on
pushes and pull requests.

### Adding New Packages

Create a new library package:
```bash
riot new --lib ./packages/my-new-library
```

Create a new binary package:
```bash
riot new --bin ./packages/my-new-binary
```

Then add the new package path to `riot.toml` workspace members.

## Structure

- `packages/` - Workspace packages
- `riot.toml` - Workspace configuration
- `ocaml-toolchain.toml` - OCaml toolchain version
- `Dockerfile` - Container build template
- `.github/workflows/ci.yml` - GitHub Actions starter workflow
|}
  in
  let path = Path.(target_dir / Path.v "README.md") in
  match Fs.write content path with
  | Ok () ->
      emit ~on_event (ScaffoldCreated { path = "README.md" });
      Ok ()
  | Error _e -> Error (Failure "Failed to create README.md")

(** Create Dockerfile *)
let create_dockerfile = fun ~on_event target_dir workspace_name is_library ->
  let content =
    if is_library then
      {|# Generated by `riot init`
FROM ghcr.io/leostera/riot/riot-builder:latest

WORKDIR /app

COPY . /app

RUN riot build --release
RUN riot test
|}
    else
      {|# Generated by `riot init`
FROM ghcr.io/leostera/riot/riot-builder:latest AS build

WORKDIR /app

COPY . /app

RUN riot build --release |}
      ^ workspace_name
      ^ {|
RUN riot test

FROM alpine:latest

RUN apk add --no-cache \
    libgcc \
    libstdc++ \
    ca-certificates

COPY --from=build /app/_build/release/*/|}
      ^ workspace_name
      ^ {| /usr/local/bin/|}
      ^ workspace_name
      ^ {|

RUN addgroup -g 1000 riot && \
    adduser -D -u 1000 -G riot riot

USER riot

ENTRYPOINT ["/usr/local/bin/|}
      ^ workspace_name
      ^ {|"]
CMD ["--help"]
|}
  in
  let path = Path.(target_dir / Path.v "Dockerfile") in
  match Fs.write content path with
  | Ok () ->
      emit ~on_event (ScaffoldCreated { path = "Dockerfile" });
      Ok ()
  | Error _e -> Error (Failure "Failed to create Dockerfile")

(** Create .github/workflows/ci.yml *)
let create_ci_workflow = fun ~on_event target_dir ->
  let workflow_dir = Path.(target_dir / Path.v ".github" / Path.v "workflows") in
  let* () =
    match Fs.create_dir_all workflow_dir with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure "Failed to create .github/workflows")
  in
  let content = {|name: CI

on:
  push:
  pull_request:

jobs:
  build-and-test:
    runs-on: ubuntu-latest
    permissions:
      contents: read
    steps:
      - uses: actions/checkout@v4

      - uses: leostera/riot/docker/setup-riot@main

      - run: riot build
      - run: riot test
|}
  in
  let path = Path.(workflow_dir / Path.v "ci.yml") in
  match Fs.write content path with
  | Ok () ->
      emit ~on_event (ScaffoldCreated { path = ".github/workflows/ci.yml" });
      Ok ()
  | Error _e -> Error (Failure "Failed to create .github/workflows/ci.yml")

(** Create default package structure *)
let create_default_package = fun ~on_event target_dir workspace_name is_library ->
  let pkg_dir = Path.(target_dir / Path.v "packages" / Path.v workspace_name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let tests_dir = Path.(pkg_dir / Path.v "tests") in
  (* Create directories *)
  let* () =
    match Fs.create_dir_all src_dir with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure "Failed to create package directories")
  in
  let* () =
    match Fs.create_dir_all tests_dir with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure "Failed to create package test directory")
  in
  let module_name = package_name_to_module_name workspace_name in
  let test_file_stem = module_name_to_test_file_stem module_name in
  (* Create package riot.toml *)
  let lib_or_bin_section =
    if is_library then
      "[lib]\npath = \"src/" ^ module_name ^ ".ml\"\n"
    else
      "[[bin]]\nname = \"" ^ workspace_name ^ "\"\npath = \"src/main.ml\"\n"
  in
  let pkg_toml_content = {|[package]
name = "|} ^ workspace_name ^ {|"
version = "0.1.0"

|} ^ lib_or_bin_section ^ {|
[dependencies]
std = "*"
|}
  in
  let pkg_toml_path = Path.(pkg_dir / Path.v "riot.toml") in
  let* () =
    match Fs.write pkg_toml_content pkg_toml_path with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure "Failed to create package riot.toml")
  in
  (* Create main .ml file *)
  let ml_content =
    if is_library then
      "open Std\n\n(** Return the starter greeting for "
      ^ workspace_name
      ^ ". *)\nlet hello = fun () -> \"Hello from "
      ^ workspace_name
      ^ "\"\n"
    else
      "open Std\n\nlet () = println (" ^ module_name ^ ".hello ())\n"
  in
  let ml_path =
    if is_library then
      Path.(src_dir / Path.v (module_name ^ ".ml"))
    else
      Path.(src_dir / Path.v "main.ml")
  in
  let* () =
    match Fs.write ml_content ml_path with
    | Ok () -> Ok ()
    | Error _e ->
        Error (
          Failure (
            "Failed to create " ^ (
              if is_library then
                module_name ^ ".ml"
              else
                "main.ml"
            )
          )
        )
  in
  (* Create .mli file *)
  let mli_content =
    if is_library then
      "(** Return the starter greeting for " ^ workspace_name ^ ". *)\nval hello: unit -> string\n"
    else
      ""
  in
  let* () =
    if is_library then
      let mli_path = Path.(src_dir / Path.v (module_name ^ ".mli")) in
      match Fs.write mli_content mli_path with
      | Ok () -> Ok ()
      | Error _e -> Error (Failure ("Failed to create " ^ module_name ^ ".mli"))
    else
      Ok ()
  in
  let* () =
    if is_library then
      Ok ()
    else
      let helper_ml_content = "open Std\n\n(** Return the starter greeting for "
      ^ workspace_name
      ^ ". *)\nlet hello = fun () -> \"Hello from "
      ^ workspace_name
      ^ "\"\n" in
      let helper_mli_content = "(** Return the starter greeting for " ^ workspace_name ^ ". *)\nval hello: unit -> string\n" in
      let helper_ml_path = Path.(src_dir / Path.v (module_name ^ ".ml")) in
      let helper_mli_path = Path.(src_dir / Path.v (module_name ^ ".mli")) in
      let* () =
        match Fs.write helper_ml_content helper_ml_path with
        | Ok () -> Ok ()
        | Error _e -> Error (Failure ("Failed to create " ^ module_name ^ ".ml"))
      in
      match Fs.write helper_mli_content helper_mli_path with
      | Ok () -> Ok ()
      | Error _e -> Error (Failure ("Failed to create " ^ module_name ^ ".mli"))
  in
  let test_content = "open Std\n\nlet test_starter_greeting = fun _ctx ->\n  Test.assert_equal ~expected:\"Hello from "
  ^ workspace_name
  ^ "\" ~actual:("
  ^ module_name
  ^ ".hello ());\n  Ok ()\n\nlet tests =\n  Test.[ case \"starter greeting\" test_starter_greeting ]\n\nlet () =\n  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:\""
  ^ test_file_stem
  ^ "\" ~tests ~args) ~args:Env.args ()\n" in
  let test_path = Path.(tests_dir / Path.v (test_file_stem ^ ".ml")) in
  let* () =
    match Fs.write test_content test_path with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure ("Failed to create " ^ test_file_stem ^ ".ml"))
  in
  emit ~on_event (ScaffoldCreated { path = "packages/" ^ workspace_name ^ "/" });
  Ok ()

(** Main run function *)
let run = fun ~on_event matches ->
  let open Result in
    let open ArgParser in
      let path_arg = get_one matches "path" in
      let name_flag = get_one matches "name" in
      let cwd = Env.current_dir () |> Result.expect ~msg:"Cannot get current directory" in
      let is_library =
        if get_flag matches "bin" then
          false
        else
          true
      in
      (* Determine target directory *)
      let target_dir =
        let resolved =
          match path_arg with
          | Some p ->
              let path = Path.v p in
              if Path.is_absolute path then
                path
              else
                Path.(cwd / path)
          | None -> cwd
        in
        Path.normalize resolved
      in
      (* Determine workspace/package name *)
      let workspace_name =
        match name_flag with
        | Some name -> name
        | None -> Path.basename target_dir
      in
      (* Validate name *)
      let* validated_name = validate_name workspace_name in
      (* Create target directory if it doesn't exist *)
      let* () =
        match Fs.create_dir_all target_dir with
        | Ok () -> Ok ()
        | Error _e -> Error (Failure "Failed to create directory")
      in
      emit ~on_event (WorkspaceInitializationStarted { name = validated_name; target_dir });
      (* Create all workspace files *)
      let* () = create_workspace_toml ~on_event target_dir validated_name in
      let* () = create_toolchain_toml ~on_event target_dir in
      let* () = create_gitignore ~on_event target_dir in
      let* () = create_readme ~on_event target_dir validated_name is_library in
      let* () = create_dockerfile ~on_event target_dir validated_name is_library in
      let* () = create_ci_workflow ~on_event target_dir in
      let* () = create_default_package ~on_event target_dir validated_name is_library in
      emit
        ~on_event
        (WorkspaceInitializationCompleted {
          next_steps = next_steps ~cwd ~target_dir ~path_arg ~is_library ~workspace_name:validated_name;
          package_hints
        });
      Ok ()
