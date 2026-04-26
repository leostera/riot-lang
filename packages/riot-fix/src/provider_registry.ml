open Std

let registered_providers = cell []

let clear = fun () -> registered_providers := []

let providers = fun () -> List.reverse !registered_providers

let register_provider = fun provider -> registered_providers := provider :: !registered_providers

let register_providers = fun providers ->
  clear ();
  List.for_each providers ~fn:register_provider

let rules = fun () ->
  providers ()
  |> List.map ~fn:Provider.rules
  |> List.concat

let rule_ids = fun () ->
  rules ()
  |> List.map ~fn:Rule.id
