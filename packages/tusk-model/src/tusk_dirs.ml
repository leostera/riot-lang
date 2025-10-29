open Std

let dot_tusk =
  let home =
    match Env.home_dir () with
    | Some h -> h
    | None -> failwith "Failed to get home directory"
  in
  Path.(home / Path.v ".tusk")

let toolchains_dir toolchain_config =
  let version = toolchain_config.Toolchain_config.version in
  Path.(dot_tusk / Path.v "toolchains" / Path.v version)

let project_dir workspace =
  let project_id = Workspace.project_id workspace in
  Path.(dot_tusk / Path.v "projects" / Path.v project_id)

let ensure_created () =
  let _ = Fs.create_dir_all dot_tusk in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "projects") in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "toolchains") in
  let _ = Fs.create_dir_all Path.(dot_tusk / Path.v "bin") in
  Ok ()
