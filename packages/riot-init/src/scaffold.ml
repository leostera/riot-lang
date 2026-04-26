open Std
open Riot_model
open Std.Result.Syntax

let scaffold_package = fun ~path ~name ~is_library ->
  let src_dir = Path.(path / Path.v "src") in
  let* () =
    Fs.create_dir_all src_dir
    |> Result.map_err ~fn:(fun _ -> "Failed to create src directory")
  in
  let module_name = Names.package_module_name name in
  let module_file_stem = Names.module_name_to_file_stem module_name in
  let main_ml =
    if is_library then
      Path.(src_dir / Path.v (module_file_stem ^ ".ml"))
    else
      Path.(src_dir / Path.v "main.ml")
  in
  let main_mli = Path.(src_dir / Path.v (module_file_stem ^ ".mli")) in
  let ml_content =
    if is_library then
      "open Std\n\n(** Main module for " ^ name ^ " library *)\n"
    else
      "open Std\n\nlet main ~args:_ =\n  println \"Hello, World!\";\n  Ok ()\n\nlet () = Runtime.run ~main ~args:Env.args ()\n"
  in
  let mli_content =
    if is_library then
      Some ("(** " ^ name ^ " library interface *)\n")
    else
      None
  in
  let package_toml = Path.(path / Path.v "riot.toml") in
  let toml_content =
    "[package]\nname = \"" ^ name ^ "\"\nversion = \"0.1.0\"\n\n" ^ (
      if is_library then
        "[lib]\npath = \"src/"
        ^ module_file_stem
        ^ ".ml\"\n\n[dependencies]\nstd = \"*\"\n# Add dependencies here\n\n"
      else
        "[[bin]]\nname = \""
        ^ name
        ^ "\"\npath = \"src/main.ml\"\n\n[dependencies]\nstd = \"*\"\n# Add dependencies here\n\n"
    )
  in
  let* () =
    Fs.write ml_content main_ml
    |> Result.map_err ~fn:(fun _ -> "Failed to write package source file")
  in
  let* () =
    match mli_content with
    | None -> Ok ()
    | Some content ->
        Fs.write content main_mli
        |> Result.map_err ~fn:(fun _ -> "Failed to write package interface file")
  in
  let* () =
    Fs.write toml_content package_toml
    |> Result.map_err ~fn:(fun _ -> "Failed to write package manifest")
  in
  Ok (Path.to_string path, name)

let new_package = fun ~workspace ~path ~name ~is_library ->
  let* (created_path, created_name) = scaffold_package ~path ~name ~is_library in
  let* () = Manifest.add_workspace_member ~workspace ~path in
  Ok (created_path, created_name)

let new_standalone_package = fun ~path ~name ~is_library -> scaffold_package ~path ~name ~is_library
