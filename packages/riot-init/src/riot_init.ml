open Std
open Riot_model

let ( let* ) = Result.and_then

(** ArgParser command definition *)
let command =
  let open ArgParser in
    let open Arg in command "init"
    |> about "Initialize a new Riot workspace"
    |> args
      [
        positional "path" |> help "Path for new workspace (default: current directory)";
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

(** Create workspace riot.toml *)
let create_workspace_toml = fun target_dir workspace_name ->
  let content = {|[workspace]
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
      println "✓ Created riot.toml";
      Ok ()
  | Error _e -> Error (Failure "Failed to create riot.toml")

(** Create ocaml-toolchain.toml *)
let create_toolchain_toml = fun target_dir ->
  let content = {|[toolchain]
version = "5.5.0-riot.2"
|}
  in
  let path = Path.(target_dir / Path.v "ocaml-toolchain.toml") in
  match Fs.write content path with
  | Ok () ->
      println "✓ Created ocaml-toolchain.toml";
      Ok ()
  | Error _e -> Error (Failure "Failed to create ocaml-toolchain.toml")

(** Create .gitignore *)
let create_gitignore = fun target_dir ->
  let content = {|# Riot build artifacts
3rdparty
generated
*.install
*.bin
_build
target
/riot
.merlin
*.cmi
*.cmo
*.cma
/logs

# OCaml
*.beam
*.trace
_opam

# IDEs
.direnv
.envrc
.tmp
|}
  in
  let path = Path.(target_dir / Path.v ".gitignore") in
  match Fs.write content path with
  | Ok () ->
      println "✓ Created .gitignore";
      Ok ()
  | Error _e -> Error (Failure "Failed to create .gitignore")

(** Create README.md *)
let create_readme = fun target_dir workspace_name ->
  let content = {|# |} ^ workspace_name ^ {|

A Riot workspace for OCaml development.

## Getting Started

### Building

Build all packages:
```bash
riot build
```

### Running

Run a binary:
```bash
riot run |} ^ workspace_name ^ {|
```

### Testing

Run tests:
```bash
riot test
```

### Adding New Packages

Create a new package:
```bash
riot new packages/my-new-package
```

Then add it to `riot.toml` workspace members.

## Structure

- `packages/` - Workspace packages
- `riot.toml` - Workspace configuration
- `ocaml-toolchain.toml` - OCaml toolchain version
|}
  in
  let path = Path.(target_dir / Path.v "README.md") in
  match Fs.write content path with
  | Ok () ->
      println "✓ Created README.md";
      Ok ()
  | Error _e -> Error (Failure "Failed to create README.md")

(** Create default package structure *)
let create_default_package = fun target_dir workspace_name is_library ->
  let pkg_dir = Path.(target_dir / Path.v "packages" / Path.v workspace_name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  (* Create directories *)
  let* () =
    match Fs.create_dir_all src_dir with
    | Ok () -> Ok ()
    | Error _e -> Error (Failure "Failed to create package directories")
  in
  let module_name = package_name_to_module_name workspace_name in
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
      "open Std\n\n(** Main module for " ^ workspace_name ^ " library *)\n"
    else
      "open Std\n\nlet () = println \"Hello, World!\"\n"
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
      "(** " ^ workspace_name ^ " library interface *)\n"
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
  println ("✓ Created packages/" ^ workspace_name ^ "/");
  Ok ()

(** Main run function *)
let run = fun matches ->
  let open Result in
    let open ArgParser in
      let path_arg = get_one matches "path" in
      let name_flag = get_one matches "name" in
      let is_library =
        if get_flag matches "bin" then
          false
        else
          true
      in
      (* Determine target directory *)
      let target_dir =
        match path_arg with
        | Some p ->
            let path =
              match Path.of_string p with
              | Ok p -> p
              | Error _ -> Path.v p
            in
            if Path.is_absolute path then
              path
            else
              let cwd = Env.current_dir () |> Result.expect ~msg:"Cannot get cwd" in
              Path.(cwd / path)
        | None -> Env.current_dir () |> Result.expect ~msg:"Cannot get current directory"
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
      (* Display what we're doing *)
      println "";
      println ("Creating workspace '" ^ validated_name ^ "' in '" ^ Path.to_string target_dir ^ "'");
      println "";
      (* Create all workspace files *)
      let* () = create_workspace_toml target_dir validated_name in
      let* () = create_toolchain_toml target_dir in
      let* () = create_gitignore target_dir in
      let* () = create_readme target_dir validated_name in
      let* () = create_default_package target_dir validated_name is_library in
      (* Success message *)
      println "";
      println "✓ Workspace initialized successfully!";
      println "";
      println "Next steps:";
      (
        match path_arg with
        | Some _ -> println ("  cd " ^ Path.to_string target_dir)
        | None -> ()
      );
      println "  riot build";
      println ("  riot run " ^ validated_name);
      println "";
      Ok ()
