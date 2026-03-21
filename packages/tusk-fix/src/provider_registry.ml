open Std

let registered_providers = cell []

let clear () =
  registered_providers := []

let providers () = List.rev !registered_providers

let register_provider provider =
  registered_providers := provider :: !registered_providers

let register_providers providers =
  clear ();
  List.iter register_provider providers

let rules () =
  providers ()
  |> List.concat_map Provider.rules

let rule_ids () =
  rules () |> List.map Rule.id
