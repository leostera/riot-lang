open Std

module Test = Std.Test
module Sparse = Pkgs_ml.Sparse_index

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "{}";
      "{\"schema_version\":1,\"kind\":\"sparse\"}";
      "{\"schema_version\":1,\"name\":\"std\",\"latest\":\"0.0.1\",\"updated_at\":\"2026-01-01T00:00:00Z\",\"releases\":[]}";
      "std";
      "pkgs.ml";
    ])

let test_pkgs_ml_fuzz = fun _ctx input ->
  Sparse.normalized_name input
  |> ignore;
  Sparse.package_prefix input
  |> ignore;
  Sparse.package_relpath input
  |> ignore;
  Sparse.bootstrap_config_url ~registry_name:input
  |> ignore;
  Sparse.config_of_string input
  |> ignore;
  Sparse.package_document_of_string input
  |> ignore;
  Ok ()

let tests =
  Test.[
    fuzz
      "pkgs-ml sparse index parsers accept arbitrary text"
      ~seeds:[
        "";
        "{}";
        "{\"schema_version\":1,\"kind\":\"sparse\",\"package_path_strategy\":\"prefix\",\"index_base_url\":\"https://cdn.pkgs.ml/index/v1\",\"artifact_base_url\":\"https://cdn.pkgs.ml\"}";
      ]
      ~mutator
      test_pkgs_ml_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"pkgs_ml_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
