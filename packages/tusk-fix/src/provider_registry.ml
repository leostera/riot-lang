open Std

let registered_providers = cell []

let clear () =
  registered_providers := [];
  Diagnostic_code.clear_package_codes ()

let providers () = List.rev !registered_providers

let register_provider provider =
  Diagnostic_code.register_package_codes (Provider.diagnostic_codes provider);
  registered_providers := provider :: !registered_providers

let register_providers providers =
  clear ();
  List.iter register_provider providers

let rules () =
  providers ()
  |> List.concat_map Provider.rules

let rule_ids () =
  rules () |> List.map Rule.id
