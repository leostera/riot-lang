open Std

let target = fun value ->
  Riot_model.Target.from_string value
  |> Result.expect ~msg:("invalid target triple: " ^ value)

let target_strings = fun set ->
  Riot_model.Target.Set.to_list set
  |> List.map ~fn:Riot_model.Target.to_string

let make_toolchain_config = fun targets ->
  Riot_model.Toolchain_config.{
    version = Riot_model.Toolchain_config.default_ocaml_version;
    source = Version Riot_model.Toolchain_config.default_ocaml_version;
    targets = List.map targets ~fn:target;
  }

let test_parse_normalizes_request_aliases = fun _ctx ->
  match (
    Riot_model.Target.parse "HOST",
    Riot_model.Target.parse "NaTiVe",
    Riot_model.Target.parse "ALL",
    Riot_model.Target.parse "linux"
  ) with
  | (
      Riot_model.Target.Host,
      Riot_model.Target.Host,
      Riot_model.Target.All,
      Riot_model.Target.Pattern "linux"
    ) -> Ok ()
  | _ -> Error "expected target request aliases and patterns to normalize case"

let test_parse_uses_exact_for_valid_target_triples = fun _ctx ->
  match Riot_model.Target.parse "x86_64-unknown-linux-gnu" with
  | Riot_model.Target.Exact targets ->
      if target_strings targets = [ "x86_64-unknown-linux-gnu" ] then
        Ok ()
      else
        Error "expected exact target request to preserve the parsed triple"
  | Riot_model.Target.Host
  | Riot_model.Target.All
  | Riot_model.Target.Pattern _ ->
      Error "expected a valid target triple to parse as an Exact request"

let test_set_deduplicates_targets = fun _ctx ->
  let set =
    Riot_model.Target.Set.from_list
      [
        target "x86_64-unknown-linux-gnu";
        target "x86_64-unknown-linux-gnu";
        target "aarch64-unknown-linux-gnu";
      ]
  in
  if target_strings set = [ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ] then
    Ok ()
  else
    Error "expected target set to deduplicate equal target triples"

let test_configured_targets_defaults_to_host = fun _ctx ->
  let host = Riot_model.Target.current in
  let config = make_toolchain_config [] in
  let configured_targets = Riot_model.Target.configured_targets ~host config in
  if target_strings configured_targets = [ Riot_model.Target.to_string host ] then
    Ok ()
  else
    Error "expected configured targets to default to the host target"

let test_configured_targets_preserves_typed_values = fun _ctx ->
  let host = Riot_model.Target.current in
  let config = make_toolchain_config [ "x86_64-unknown-linux-gnu"; "aarch64-unknown-linux-gnu" ] in
  let configured_targets = Riot_model.Target.configured_targets ~host config in
  if
    target_strings configured_targets = [ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ]
  then
    Ok ()
  else
    Error "expected configured targets to preserve typed toolchain targets"

let test_configured_targets_deduplicates_repeated_values = fun _ctx ->
  let host = Riot_model.Target.current in
  let config = make_toolchain_config [ "x86_64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ] in
  let configured_targets = Riot_model.Target.configured_targets ~host config in
  if target_strings configured_targets = [ "x86_64-unknown-linux-gnu" ] then
    Ok ()
  else
    Error "expected configured targets to deduplicate repeated typed target triples"

let test_resolve_host_uses_the_host_target = fun _ctx ->
  let host = target "aarch64-apple-darwin" in
  let configured_targets = Riot_model.Target.make_set [ host; target "x86_64-unknown-linux-gnu" ] in
  match Riot_model.Target.resolve ~host ~configured_targets Riot_model.Target.Host with
  | Ok targets when target_strings targets = [ "aarch64-apple-darwin" ] -> Ok ()
  | Ok _ -> Error "expected Host request to resolve to the host target only"
  | Error _ -> Error "expected Host request to resolve successfully"

let test_resolve_all_uses_all_configured_targets = fun _ctx ->
  let host = target "aarch64-apple-darwin" in
  let configured_targets = Riot_model.Target.make_set [ host; target "x86_64-unknown-linux-gnu" ] in
  match Riot_model.Target.resolve ~host ~configured_targets Riot_model.Target.All with
  | Ok targets when target_strings targets = [ "aarch64-apple-darwin"; "x86_64-unknown-linux-gnu" ] ->
      Ok ()
  | Ok _ -> Error "expected All request to resolve to every configured target"
  | Error _ -> Error "expected All request to resolve successfully"

let test_resolve_pattern_matches_substrings = fun _ctx ->
  let host = target "aarch64-apple-darwin" in
  let configured_targets =
    Riot_model.Target.make_set
      [ host; target "x86_64-unknown-linux-gnu"; target "aarch64-unknown-linux-gnu" ]
  in
  match Riot_model.Target.resolve ~host ~configured_targets (Riot_model.Target.Pattern "linux") with
  | Ok targets when target_strings targets
  = [ "aarch64-unknown-linux-gnu"; "x86_64-unknown-linux-gnu" ] -> Ok ()
  | Ok _ -> Error "expected pattern request to resolve matching configured targets"
  | Error _ -> Error "expected pattern request to resolve successfully"

let test_resolve_reports_available_targets_on_miss = fun _ctx ->
  let host = target "aarch64-apple-darwin" in
  let configured_targets = Riot_model.Target.make_set [ host; target "x86_64-unknown-linux-gnu" ] in
  match Riot_model.Target.resolve ~host ~configured_targets (Riot_model.Target.Pattern "windows") with
  | Error { pattern; available_targets } ->
      if not (String.equal pattern "windows") then
        Error "expected target miss pattern to be preserved"
      else
        let actual = List.map available_targets ~fn:Riot_model.Target.to_string in
        if
          not
            (String.equal (String.concat "," actual) "aarch64-apple-darwin,x86_64-unknown-linux-gnu")
        then
          Error "expected target miss to expose available configured targets"
        else
          Ok ()
  | Ok _ -> Error "expected target miss to fail with available targets"

let tests =
  Test.[
    case "Target.parse normalizes request aliases" test_parse_normalizes_request_aliases;
    case
      "Target.parse uses Exact for valid target triples"
      test_parse_uses_exact_for_valid_target_triples;
    case "Target.Set deduplicates equal target triples" test_set_deduplicates_targets;
    case "Target.configured_targets defaults to host" test_configured_targets_defaults_to_host;
    case
      "Target.configured_targets preserves typed configured values"
      test_configured_targets_preserves_typed_values;
    case
      "Target.configured_targets deduplicates repeated configured values"
      test_configured_targets_deduplicates_repeated_values;
    case "Target.resolve Host uses the host target" test_resolve_host_uses_the_host_target;
    case
      "Target.resolve All uses all configured targets"
      test_resolve_all_uses_all_configured_targets;
    case
      "Target.resolve pattern requests match configured target substrings"
      test_resolve_pattern_matches_substrings;
    case
      "Target.resolve reports available targets on a miss"
      test_resolve_reports_available_targets_on_miss;
  ]

let main ~args = Test.Cli.main ~name:"target" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
