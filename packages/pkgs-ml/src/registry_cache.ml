open Std

type t = {
  tusk_home: Path.t;
  registry_name: string;
}

let create = fun ?tusk_home ~registry_name () ->
  match tusk_home with
  | Some tusk_home -> Ok { tusk_home; registry_name }
  | None -> (
      match Env.home_dir () with
      | Some home ->
          Ok {
            tusk_home =
              Path.(home / Path.v ".tusk");
            registry_name;
          }
      | None -> Error "failed to determine home directory for pkgs.ml cache"
    )

let tusk_home = fun cache -> cache.tusk_home

let registry_name = fun cache -> cache.registry_name

let registry_dir = fun cache ->
  Path.(cache.tusk_home / Path.v "registry" / Path.v cache.registry_name)

let index_dir = fun cache -> Path.(registry_dir cache / Path.v "index")

let archive_dir = fun cache -> Path.(registry_dir cache / Path.v "archive")

let archive_path = fun cache ~package_name ~version ->
  Path.(archive_dir cache / Path.v package_name / Path.v (version ^ ".tar"))

let src_dir = fun cache -> Path.(registry_dir cache / Path.v "src")

let package_src_dir = fun cache ~package_name ~version ->
  Path.(src_dir cache / Path.v package_name / Path.v version)

module Tests = struct
  let test_registry_split_layout () : (unit, string) result =
    let cache = create ~tusk_home:(Path.v "/tmp/.tusk") ~registry_name:"pkgs.ml" () |> Result.unwrap in
    let index = index_dir cache |> Path.to_string in
    let archive = archive_path cache ~package_name:"std" ~version:"0.1.0" |> Path.to_string in
    let src = package_src_dir cache ~package_name:"std" ~version:"0.1.0" |> Path.to_string in
    if
      String.equal index "/tmp/.tusk/registry/pkgs.ml/index"
      && String.equal archive "/tmp/.tusk/registry/pkgs.ml/archive/std/0.1.0.tar"
      && String.equal src "/tmp/.tusk/registry/pkgs.ml/src/std/0.1.0"
    then
      Ok ()
    else
      Error ("unexpected registry layout:\nindex=" ^ index ^ "\narchive=" ^ archive ^ "\nsrc=" ^ src) [@test]
end [@test]
