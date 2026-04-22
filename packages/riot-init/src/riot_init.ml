open Std
open Riot_model
open Std.Result.Syntax

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

(** Validate workspace name separately from package naming rules. *)
let validate_workspace_name = fun name ->
  if String.length name = 0 then
    Error (Failure "Workspace name cannot be empty")
  else
    Ok name

(** Normalize a workspace name into a valid starter package name. *)
let starter_package_name = fun workspace_name ->
  String.map
    ~fn:(fun c ->
      if c = '.' then
        '-'
      else
        c)
    workspace_name

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

let package_module_name = fun name ->
  String.split ~by:"-" name |> List.map ~fn:String.capitalize_ascii |> String.concat ""

let find_substring = fun ~needle ~start haystack ->
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop index =
    if index + needle_len > haystack_len then
      None
    else if String.equal (String.sub haystack ~offset:index ~len:needle_len) needle then
      Some index
    else
      loop (index + 1)
  in
  loop start

let find_char_from = fun ~char ~start source ->
  let rec loop index =
    if index >= String.length source then
      None
    else if Char.equal (String.get_unchecked source ~at:index) char then
      Some index
    else
      loop (index + 1)
  in
  loop start

let workspace_manifest_path = fun (workspace: Workspace_manifest.t) ->
  Path.(workspace.root / Path.v "riot.toml")

let relative_member_path = fun ~(workspace:Workspace_manifest.t) path ->
  let normalized_path = Path.normalize path in
  if Path.is_absolute normalized_path then
    Path.strip_prefix normalized_path ~prefix:(Path.normalize workspace.root)
    |> Result.map ~fn:Path.normalize
    |> Result.map_err ~fn:(fun _ -> "Package path must live under the workspace root")
  else
    Ok (Path.normalize normalized_path)

let add_workspace_member_to_source = fun ~member source ->
  let quoted_member = "\"" ^ member ^ "\"" in
  if String.contains source quoted_member then
    Ok source
  else
    let* workspace_index =
      match find_substring ~needle:"[workspace]" ~start:0 source with
      | Some index -> Ok index
      | None -> Error "Failed to find [workspace] section in riot.toml"
    in
    let* members_index =
      match find_substring ~needle:"members" ~start:workspace_index source with
      | Some index -> Ok index
      | None -> Error "Failed to find workspace members in riot.toml"
    in
    let* open_index =
      match find_char_from ~char:'[' ~start:members_index source with
      | Some index -> Ok index
      | None -> Error "Failed to parse workspace members in riot.toml"
    in
    let* close_index =
      match find_char_from ~char:']' ~start:(open_index + 1) source with
      | Some index -> Ok index
      | None -> Error "Failed to find the end of workspace members in riot.toml"
    in
    let before_members = String.sub source ~offset:0 ~len:(open_index + 1) in
    let members_body = String.sub source ~offset:(open_index + 1) ~len:(close_index - open_index - 1) in
    let after_members = String.sub
      source
      ~offset:close_index
      ~len:(String.length source - close_index) in
    let inserted_body =
      if String.is_empty (String.trim members_body) then
        "\n  " ^ quoted_member ^ ",\n"
      else if String.contains members_body "\n" then
        members_body ^ "  " ^ quoted_member ^ ",\n"
      else
        "\n  " ^ String.trim members_body ^ ",\n  " ^ quoted_member ^ ",\n"
    in
    Ok (before_members ^ inserted_body ^ after_members)

let add_workspace_member = fun ~(workspace:Workspace_manifest.t) ~path ->
  let* relative_path = relative_member_path ~workspace path in
  let manifest_path = workspace_manifest_path workspace in
  let* manifest_source = Fs.read_to_string manifest_path |> Result.map_err ~fn:IO.error_message in
  let* updated_source = add_workspace_member_to_source ~member:(Path.to_string relative_path) manifest_source in
  Fs.write updated_source manifest_path
  |> Result.map_err ~fn:(fun err -> "Failed to update workspace manifest: " ^ IO.error_message err)

let scaffold_package = fun ~path ~name ~is_library ->
  let src_dir = Path.(path / Path.v "src") in
  let* () = Fs.create_dir_all src_dir
  |> Result.map_err ~fn:(fun _ -> "Failed to create src directory") in
  let module_name = package_module_name name in
  let main_ml =
    if is_library then
      Path.(src_dir / Path.v (module_name ^ ".ml"))
    else
      Path.(src_dir / Path.v "main.ml")
  in
  let main_mli = Path.(src_dir / Path.v (module_name ^ ".mli")) in
  let ml_content =
    if is_library then
      "open Std\n\n(** Main module for " ^ name ^ " library *)\n"
    else
      "open Std\n\nlet main = fun ~args:_ ->\n  println \"Hello, World!\";\n  Ok ()\n\nlet () = Actors.run ~main ~args:Env.args ()\n"
  in
  let mli_content =
    if is_library then
      Some ("(** " ^ name ^ " library interface *)\n")
    else
      None
  in
  let package_toml = Path.(path / Path.v "riot.toml") in
  let toml_content = "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.1.0\"\n\n" ^ (
    if is_library then
      "[lib]\npath = \"src/" ^ module_name ^ ".ml\"\n\n"
    else
      "[[bin]]\nname = \"" ^ name ^ "\"\npath = \"src/main.ml\"\n\n"
  ) ^ "[dependencies]\nstd = \"*\"\n# Add dependencies here\n\n"
  in
  let* () = Fs.write ml_content main_ml
  |> Result.map_err ~fn:(fun _ -> "Failed to write package source file") in
  let* () =
    match mli_content with
    | None -> Ok ()
    | Some content -> Fs.write content main_mli
    |> Result.map_err ~fn:(fun _ -> "Failed to write package interface file")
  in
  let* () = Fs.write toml_content package_toml
  |> Result.map_err ~fn:(fun _ -> "Failed to write package manifest") in
  Ok (Path.to_string path, name)

let new_package = fun ~workspace ~path ~name ~is_library ->
  let* (created_path, created_name) = scaffold_package ~path ~name ~is_library in
  let* () = add_workspace_member ~workspace ~path in
  Ok (created_path, created_name)

let new_standalone_package = fun ~path ~name ~is_library -> scaffold_package ~path ~name ~is_library

let package_hints = [
  (Library, "riot new --lib ./packages/<name>");
  (Binary, "riot new --bin ./packages/<name>");
]

let next_steps = fun ~cwd ~target_dir ~path_arg ~is_library ~package_name ->
  let steps = ref [] in
  if Option.is_some path_arg && not (Path.equal cwd target_dir) then
    steps := !steps @ [ "cd " ^ Path.to_string target_dir ];
  steps := !steps @ [ "riot build"; "riot test" ];
  if not is_library then
    steps := !steps @ [ "riot run " ^ package_name ];
  !steps

let emit = fun ~on_event event -> on_event event

(** Create workspace riot.toml *)
let create_workspace_toml = fun ~on_event target_dir workspace_name package_name ->
  let content = {|[workspace]
name = "|} ^ workspace_name ^ {|"
members = [
  "packages/|} ^ package_name ^ {|",
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
let create_readme = fun ~on_event target_dir workspace_name package_name is_library ->
  let running_section =
    if is_library then
      ""
    else
      {|
### Running

Run the starter binary:
```bash
riot run |} ^ package_name ^ {|
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
let create_dockerfile = fun ~on_event target_dir workspace_name package_name is_library ->
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

RUN riot build --release |} ^ workspace_name ^ {|
RUN riot test

FROM alpine:latest

RUN apk add --no-cache \
    libgcc \
    libstdc++ \
    ca-certificates

COPY --from=build /app/_build/release/*/|} ^ package_name ^ {| /usr/local/bin/|} ^ package_name ^ {|

RUN addgroup -g 1000 riot && \
    adduser -D -u 1000 -G riot riot

USER riot

ENTRYPOINT ["/usr/local/bin/|} ^ package_name ^ {|"]
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
let create_default_package = fun ~on_event target_dir workspace_name package_name is_library ->
  let pkg_dir = Path.(target_dir / Path.v "packages" / Path.v package_name) in
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
  let module_name = package_name_to_module_name package_name in
  let test_file_stem = module_name_to_test_file_stem module_name in
  (* Create package riot.toml *)
  let lib_or_bin_section =
    if is_library then
      "[lib]\npath = \"src/" ^ module_name ^ ".ml\"\n"
    else
      "[[bin]]\nname = \"" ^ package_name ^ "\"\npath = \"src/main.ml\"\n"
  in
  let pkg_toml_content = {|[package]
name = "|} ^ package_name ^ {|"
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
      "open Std\n\nlet main = fun ~args:_ ->\n  println (" ^ module_name ^ ".hello ());\n  Ok ()\n\nlet () = Actors.run ~main ~args:Env.args ()\n"
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
  ^ "\" ~tests ~args ()) ~args:Env.args ()\n" in
  let test_path = Path.(tests_dir / Path.v (test_file_stem ^ ".ml")) in
  let* () =
    match Fs.write test_content test_path with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure ("Failed to create " ^ test_file_stem ^ ".ml"))
  in
  emit ~on_event (ScaffoldCreated { path = "packages/" ^ package_name ^ "/" });
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
      let* workspace_name = validate_workspace_name workspace_name in
      let package_name = starter_package_name workspace_name in
      let* validated_package_name = validate_name package_name in
      let validated_package_name = Package_name.to_string validated_package_name in
      (* Create target directory if it doesn't exist *)
      let* () =
        match Fs.create_dir_all target_dir with
        | Ok () -> Ok ()
        | Error _e -> Error (Failure "Failed to create directory")
      in
      emit ~on_event (WorkspaceInitializationStarted { name = workspace_name; target_dir });
      (* Create all workspace files *)
      let* () = create_workspace_toml ~on_event target_dir workspace_name validated_package_name in
      let* () = create_toolchain_toml ~on_event target_dir in
      let* () = create_gitignore ~on_event target_dir in
      let* () = create_readme ~on_event target_dir workspace_name validated_package_name is_library in
      let* () = create_dockerfile ~on_event target_dir workspace_name validated_package_name is_library in
      let* () = create_ci_workflow ~on_event target_dir in
      let* () = create_default_package ~on_event target_dir workspace_name validated_package_name is_library in
      emit
        ~on_event
        (WorkspaceInitializationCompleted {
          next_steps = next_steps ~cwd ~target_dir ~path_arg ~is_library ~package_name:validated_package_name;
          package_hints
        });
      Ok ()
