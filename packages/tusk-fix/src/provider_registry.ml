open Std

let registered_providers = cell []

let clear = fun () -> registered_providers := []

let providers = fun () -> List.rev !registered_providers

let register_provider = fun provider -> registered_providers := provider :: !registered_providers

let register_providers = fun providers ->
    clear ();
    List.iter register_provider providers

let rules = fun () -> providers () |> List.concat_map Provider.rules

let rule_ids = fun () -> rules () |> List.map Rule.id
