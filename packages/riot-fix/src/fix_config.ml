open Std
open Std.Collections
open Riot_model

type rule_state =
  Enabled
  | Disabled

type rule_override = {
  name: string;
  state: rule_state;
}

type fix_config = {
  ignore_patterns: string list;
  rules: rule_override list;
}

type package_scope = {
  package_root: Path.t;
  config: fix_config;
}

type scope = {
  workspace_root: Path.t;
  target_dir_root: Path.t;
  workspace_config: fix_config;
  packages: package_scope list;
  providers: Riot_model.Fix_provider.t list;
}

let empty_fix_config = { ignore_patterns = []; rules = [] }

let parse_rule_state state =
  match state with
  | "enabled" -> Some Enabled
  | "disabled" -> Some Disabled
  | _ -> None

let parse_rule_override item =
  match item with
  | Data.Toml.String name ->
      if String.starts_with ~prefix:"-" name then
        Some { name = String.sub name 1 (String.length name - 1); state = Disabled }
      else
        Some { name; state = Enabled }
  | Data.Toml.Table attrs -> (
      match List.assoc_opt "name" attrs with
      | Some (Data.Toml.String name) ->
          let state =
            match List.assoc_opt "state" attrs with
            | Some (Data.Toml.String state) -> Option.unwrap_or
              ~default:Enabled (parse_rule_state state)
            | _ -> Enabled
          in
          Some { name; state }
      | _ -> None
    )
  | _ ->
      None

let parse_fix_config toml =
  match toml with
  | Data.Toml.Table items -> (
      match List.assoc_opt "riot" items with
      | Some (Data.Toml.Table riot_items) -> (
          match List.assoc_opt "fix" riot_items with
          | Some (Data.Toml.Table fix_items) ->
              let ignore_patterns =
                match List.assoc_opt "ignore" fix_items with
                | Some (Data.Toml.Array items) -> List.filter_map Data.Toml.get_string items
                | _ -> []
              in
              let rules =
                match List.assoc_opt "rules" fix_items with
                | Some (Data.Toml.Array items) -> List.filter_map parse_rule_override items
                | _ -> []
              in
              { ignore_patterns; rules }
          | _ -> empty_fix_config
        )
      | _ -> empty_fix_config
    )
  | _ -> empty_fix_config

let load_fix_config = fun path ->
  match Fs.read_to_string path with
  | Error _ -> empty_fix_config
  | Ok content -> (
      match Data.Toml.parse content with
      | Ok toml -> parse_fix_config toml
      | Error _ -> empty_fix_config
    )

let load_scope = fun ~cwd ->
  match Workspace_manager.scan cwd with
  | Error _ -> None
  | Ok (workspace, _load_errors) ->
      let workspace_toml = Path.(workspace.root / Path.v "riot.toml") in
      let workspace_config = load_fix_config workspace_toml in
      let packages =
        workspace.packages
        |> List.filter Package.is_workspace_member
        |> List.map
          (fun (pkg: Package.t) ->
            let package_toml = Path.(pkg.path / Path.v "riot.toml") in
            { package_root = pkg.path; config = load_fix_config package_toml })
      in
      Some {
        workspace_root = workspace.root;
        target_dir_root = workspace.target_dir_root;
        workspace_config;
        packages;
        providers = Workspace.discover_fix_providers workspace;
      }

let workspace_root = fun scope -> scope.workspace_root

let target_dir_root = fun scope -> scope.target_dir_root

let providers scope =
  match scope with
  | None -> []
  | Some scope -> scope.providers

let ignore_patterns scope =
  match scope with
  | None -> []
  | Some scope -> scope.workspace_config.ignore_patterns

let rec glob_match = fun pattern text ->
  if String.equal pattern "" then
    String.equal text ""
  else if String.get pattern 0 = '*' then
    let rest = String.sub pattern 1 (String.length pattern - 1) in
    String.equal rest ""
    || glob_match rest text
    || (String.length text > 0 && glob_match pattern (String.sub text 1 (String.length text - 1)))
  else
    String.length text > 0
    && String.get pattern 0 = String.get text 0
    && glob_match
      (String.sub pattern 1 (String.length pattern - 1))
      (String.sub text 1 (String.length text - 1))

let matches_pattern = fun pattern candidate ->
  if String.contains pattern "*" then
    glob_match pattern candidate
  else
    String.equal pattern candidate
    || String.equal pattern (Path.basename (Path.v candidate))
    || String.contains candidate pattern

let find_package_scope = fun scope file ->
  scope.packages |> List.filter_map
    (fun package_scope ->
      match Path.strip_prefix file ~prefix:package_scope.package_root with
      | Ok _ -> Some (String.length (Path.to_string package_scope.package_root), package_scope)
      | Error _ -> None) |> List.sort
    (fun ((left_len, _)) ((right_len, _)) ->
      Int.compare right_len left_len) |> List.map snd |> function
  | package_scope :: _ -> Some package_scope
  | [] -> None

let matches_ignore_patterns = fun file patterns ->
  let path = Path.to_string file in
  List.exists (fun pattern -> matches_pattern pattern path) patterns

let should_ignore_file = fun scope file ->
  match scope with
  | None -> false
  | Some scope ->
      let workspace_ignored = matches_ignore_patterns file scope.workspace_config.ignore_patterns in
      if workspace_ignored then
        true
      else
        match find_package_scope scope file with
        | Some package_scope -> matches_ignore_patterns file package_scope.config.ignore_patterns
        | None -> false

let set_rule_state = fun states name enabled ->
  (name, enabled) :: List.filter (fun ((existing, _)) -> not (String.equal existing name)) states

let matching_rule_names = fun states name ->
  if String.contains name ":" then
    [ name ]
  else
    let names = List.map fst states in
    let exact_matches =
      List.filter
        (fun actual ->
          String.equal actual name)
        names
    in
    if not (List.is_empty exact_matches) then
      exact_matches
    else
      let suffix = ":" ^ name in
      let qualified_matches = List.filter (String.ends_with ~suffix) names in
      if not (List.is_empty qualified_matches) then
        qualified_matches
      else
        [ name ]

let apply_rule_overrides = fun states overrides ->
  List.fold_left
    (fun acc rule_override ->
      let enabled =
        match rule_override.state with
        | Enabled -> true
        | Disabled -> false
      in
      matching_rule_names acc rule_override.name
      |> List.fold_left (fun acc rule_name -> set_rule_state acc rule_name enabled) acc)
    states
    overrides

let default_rule_states = fun () ->
  Pipeline.default_rule_ids () |> List.map (fun name -> (name, true))

let effective_rule_states = fun scope file ->
  match scope with
  | None -> default_rule_states ()
  | Some scope ->
      let base_states = default_rule_states ()
      |> fun states -> apply_rule_overrides states scope.workspace_config.rules in
      match find_package_scope scope file with
      | Some package_scope -> apply_rule_overrides base_states package_scope.config.rules
      | None -> base_states

let pipeline_for_file = fun scope file ->
  let enabled_rule_ids =
    effective_rule_states scope file
    |> List.filter_map
      (fun ((name, enabled)) ->
        if enabled then
          Some name
        else
          None)
  in
  Pipeline.make ~rules:(Pipeline.rules_by_id enabled_rule_ids) ()
