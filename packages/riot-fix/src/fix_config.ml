open Std
open Std.Collections
open Riot_model

type rule_state =
  | Enabled
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
  workspace: Riot_model.Workspace_manifest.t;
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
        Some { name = String.sub name ~offset:1 ~len:(String.length name - 1); state = Disabled }
      else
        Some { name; state = Enabled }
  | Data.Toml.Table attrs -> (
      let name_value =
        List.find attrs ~fn:(fun (attr_name, _) -> String.equal attr_name "name")
        |> Option.map ~fn:(fun (_, value) -> value)
      in
      match name_value with
      | Some (Data.Toml.String name) ->
          let state =
            let state_value =
              List.find attrs ~fn:(fun (attr_name, _) -> String.equal attr_name "state")
              |> Option.map ~fn:(fun (_, value) -> value)
            in
            match state_value with
            | Some (Data.Toml.String state) ->
                Option.unwrap_or ~default:Enabled (parse_rule_state state)
            | _ -> Enabled
          in
          Some { name; state }
      | _ -> None
    )
  | _ -> None

let parse_fix_config toml =
  match toml with
  | Data.Toml.Table items -> (
      let riot_value =
        List.find items ~fn:(fun (item_name, _) -> String.equal item_name "riot")
        |> Option.map ~fn:(fun (_, value) -> value)
      in
      match riot_value with
      | Some (Data.Toml.Table riot_items) -> (
          let fix_value =
            List.find riot_items ~fn:(fun (item_name, _) -> String.equal item_name "fix")
            |> Option.map ~fn:(fun (_, value) -> value)
          in
          match fix_value with
          | Some (Data.Toml.Table fix_items) ->
              let ignore_patterns =
                let ignore_value =
                  List.find fix_items ~fn:(fun (item_name, _) -> String.equal item_name "ignore")
                  |> Option.map ~fn:(fun (_, value) -> value)
                in
                match ignore_value with
                | Some (Data.Toml.Array items) -> List.filter_map items ~fn:Data.Toml.get_string
                | _ -> []
              in
              let rules =
                let rules_value =
                  List.find fix_items ~fn:(fun (item_name, _) -> String.equal item_name "rules")
                  |> Option.map ~fn:(fun (_, value) -> value)
                in
                match rules_value with
                | Some (Data.Toml.Array items) -> List.filter_map items ~fn:parse_rule_override
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
  let workspace_manager = Workspace_manager.create () in
  match Workspace_manager.scan workspace_manager cwd with
  | Error _ -> None
  | Ok (workspace, _load_errors) ->
      let workspace_toml = Path.(workspace.root / Path.v "riot.toml") in
      let workspace_config = load_fix_config workspace_toml in
      let packages =
        workspace.packages
        |> List.filter ~fn:Package_manifest.is_workspace_member
        |> List.map
          ~fn:(fun (pkg: Package_manifest.t) ->
            let package_toml = Path.(pkg.path / Path.v "riot.toml") in
            { package_root = pkg.path; config = load_fix_config package_toml })
      in
      Some {
        workspace;
        workspace_root = workspace.root;
        target_dir_root = workspace.target_dir_root;
        workspace_config;
        packages;
        providers = Workspace_manifest.discover_fix_providers workspace;
      }

let workspace_root = fun scope -> scope.workspace_root

let workspace = fun scope -> scope.workspace

let target_dir_root = fun scope -> scope.target_dir_root

let providers scope =
  match scope with
  | None -> []
  | Some scope -> scope.providers

let ignore_patterns scope =
  match scope with
  | None -> []
  | Some scope -> scope.workspace_config.ignore_patterns

let glob_match = fun pattern text ->
  let pattern_len = String.length pattern in
  let text_len = String.length text in
  let previous = ref (Array.make ~count:(text_len + 1) ~value:false) in
  Array.set !previous ~at:0 ~value:true;
  for pattern_idx = 1 to pattern_len do
    let current = Array.make ~count:(text_len + 1) ~value:false in
    if String.get_unchecked pattern ~at:(pattern_idx - 1) = '*' then (
      Array.set
        current
        ~at:0
        ~value:(Array.get_unchecked !previous ~at:0);
      for text_idx = 1 to text_len do
        Array.set
          current
          ~at:text_idx
          ~value:(Array.get_unchecked !previous ~at:text_idx
          || Array.get_unchecked current ~at:(text_idx - 1))
      done
    ) else (
      for text_idx = 1 to text_len do
        Array.set
          current
          ~at:text_idx
          ~value:(Array.get_unchecked !previous ~at:(text_idx - 1)
          && String.get_unchecked pattern ~at:(pattern_idx - 1)
          = String.get_unchecked text ~at:(text_idx - 1))
      done
    );
    previous := current
  done;
  Array.get_unchecked !previous ~at:text_len

let glob_match_anywhere = fun pattern text ->
  let text_len = String.length text in
  let matched = ref false in
  let text_idx = ref 0 in
  while !text_idx <= text_len && not !matched do
    matched := glob_match pattern (String.sub text ~offset:!text_idx ~len:(text_len - !text_idx));
    text_idx := !text_idx + 1
  done;
  !matched

let matches_pattern = fun pattern candidate ->
  let basename = Path.basename (Path.v candidate) in
  if String.contains pattern "*" then
    glob_match pattern candidate
    || glob_match pattern basename
    || glob_match_anywhere pattern candidate
  else
    String.equal pattern candidate
    || String.equal pattern basename
    || String.contains candidate pattern

let find_package_scope = fun scope file ->
  scope.packages
  |> List.filter_map
    ~fn:(fun package_scope ->
      match Path.strip_prefix file ~prefix:package_scope.package_root with
      | Ok _ -> Some (String.length (Path.to_string package_scope.package_root), package_scope)
      | Error _ -> None)
  |> List.sort ~compare:(fun (left_len, _) (right_len, _) -> Int.compare right_len left_len)
  |> List.map ~fn:(fun (_, package_scope) -> package_scope)
  |> fun __tmp1 ->
    match __tmp1 with
    | package_scope :: _ -> Some package_scope
    | [] -> None

let matches_ignore_patterns = fun file patterns ->
  let path = Path.to_string file in
  List.any patterns ~fn:(fun pattern -> matches_pattern pattern path)

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
  (name, enabled) :: List.filter states ~fn:(fun (existing, _) -> not (String.equal existing name))

let matching_rule_names = fun states name ->
  if String.contains name ":" then
    [ name ]
  else
    let names = List.map states ~fn:(fun (rule_name, _) -> rule_name) in
    let exact_matches = List.filter names ~fn:(fun actual -> String.equal actual name) in
    if not (List.is_empty exact_matches) then
      exact_matches
    else
      let suffix = ":" ^ name in
      let qualified_matches = List.filter names ~fn:(String.ends_with ~suffix) in
      if not (List.is_empty qualified_matches) then
        qualified_matches
      else
        [ name ]

let apply_rule_overrides = fun states overrides ->
  List.fold_left
    overrides
    ~init:states
    ~fn:(fun acc rule_override ->
      let enabled =
        match rule_override.state with
        | Enabled -> true
        | Disabled -> false
      in
      matching_rule_names acc rule_override.name
      |> List.fold_left ~init:acc ~fn:(fun acc rule_name -> set_rule_state acc rule_name enabled))

let default_rule_states = fun () ->
  Pipeline.default_rule_ids ()
  |> List.map ~fn:(fun name -> (Rule_id.to_string name, true))

let effective_rule_states = fun scope file ->
  match scope with
  | None -> default_rule_states ()
  | Some scope ->
      let base_states =
        default_rule_states ()
        |> fun states -> apply_rule_overrides states scope.workspace_config.rules
      in
      match find_package_scope scope file with
      | Some package_scope -> apply_rule_overrides base_states package_scope.config.rules
      | None -> base_states

let pipeline_for_file = fun scope file ->
  let enabled_rule_ids =
    effective_rule_states scope file
    |> List.filter_map
      ~fn:(fun (name, enabled) ->
        if enabled then
          Some (Rule_id.from_string name)
        else
          None)
  in
  Pipeline.make ~rules:(Pipeline.rules_by_id enabled_rule_ids) ()
