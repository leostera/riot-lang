open Std

let sparse_config_source = {|{
  "schema_version": 1,
  "kind": "sparse",
  "package_path_strategy": "cargo-lowercase-v1",
  "index_base_url": "https://cdn.pkgs.ml/index/v1",
  "artifact_base_url": "https://cdn.pkgs.ml"
}|}

let main = fun ~args:_ ->
  let cache = Pkgs_ml.Registry_cache.create
    ~riot_home:(Path.v "/tmp/pkgs-ml-example")
    ~registry_name:"pkgs.ml"
    ()
  |> Result.expect ~msg:"cache should be creatable"
  in
  let config = Pkgs_ml.Sparse_index.config_of_string sparse_config_source
  |> Result.expect ~msg:"example config should parse"
  in
  let package_url = Pkgs_ml.Sparse_index.package_document_url config ~package_name:"Std"
  |> Result.expect ~msg:"package url should be derivable"
  in
  println ("index dir: " ^ Path.to_string (Pkgs_ml.Registry_cache.index_dir cache));
  println
    ("archive path: "
    ^ Path.to_string (Pkgs_ml.Registry_cache.archive_path cache ~package_name:"std" ~version:"0.1.0"));
  println
    ("source dir: "
    ^ Path.to_string (Pkgs_ml.Registry_cache.package_src_dir cache ~package_name:"std" ~version:"0.1.0"));
  println ("package document url: " ^ Net.Uri.to_string package_url);
  Ok ()

let () = Actors.run ~main ~args:Env.args ()
