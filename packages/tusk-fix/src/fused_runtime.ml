open Std

type plan = {
  provider_hash : string;
  generated_dir : Path.t;
  registry_path : Path.t;
  package_name : string;
  binary_name : string;
}

let provider_fingerprint (provider : Tusk_model.Fix_provider.t) =
  String.concat ":"
    [
      provider.package_name;
      provider.name;
      provider.module_name;
      String.concat "," provider.rules;
    ]

let provider_hash providers =
  providers
  |> List.sort (fun (left : Tusk_model.Fix_provider.t) right ->
         String.compare (provider_fingerprint left) (provider_fingerprint right))
  |> List.map provider_fingerprint
  |> String.concat "\n"
  |> Crypto.hash_string
  |> Crypto.Digest.hex

let plan ~target_dir_root providers =
  let hash = provider_hash providers in
  let generated_dir =
    Path.(target_dir_root / Path.v "tusk-fix" / Path.v "fused" / Path.v hash)
  in
  {
    provider_hash = hash;
    generated_dir;
    registry_path = Path.(generated_dir / Path.v "src" / Path.v "fused_registry.ml");
    package_name = "tusk-fix-fused";
    binary_name = "tusk-fix-fused";
  }

let provider_module_line (provider : Tusk_model.Fix_provider.t) =
  "  (module " ^ provider.module_name ^ " : Tusk_fix_api.Provider);"

let registry_source providers =
  String.concat "\n"
    [
      "open Std";
      "";
      "let providers : (module Tusk_fix_api.Provider) list = [";
      String.concat "\n" (List.map provider_module_line providers);
      "]";
      "";
    ]
