open Std
module Test = Std.Test

let test_registry_split_layout = fun () ->
  let cache =
    Pkgs_ml.Registry_cache.create
      ~tusk_home:(Path.v "/tmp/.tusk")
      ~registry_name:"pkgs.ml"
    |> Result.expect ~msg:"expected registry cache to be created"
  in
  let index = Pkgs_ml.Registry_cache.index_dir cache |> Path.to_string in
  let archive =
    Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0"
    |> Path.to_string
  in
  let src =
    Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"
    |> Path.to_string
  in
  if
    String.equal index "/tmp/.tusk/registry/pkgs.ml/index"
    && String.equal archive "/tmp/.tusk/registry/pkgs.ml/archive/std/0.1.0.tar"
    && String.equal src "/tmp/.tusk/registry/pkgs.ml/src/std/0.1.0"
  then
    Ok ()
  else
    Error
      ("unexpected registry layout:\nindex="
      ^ index
      ^ "\narchive="
      ^ archive
      ^ "\nsrc="
      ^ src)

let tests =
  Test.[
    case "registry cache: uses cargo-style split layout" test_registry_split_layout;
  ]

let name = "pkgs-ml Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
