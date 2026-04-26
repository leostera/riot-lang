open Std
open Riot_model
open Std.Result.Syntax

let find_substring = fun ~needle ~start haystack ->
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop index =
    if index + needle_len > haystack_len then
      None
    else if String.equal (String.sub haystack ~offset:index ~len:needle_len) needle then
      Some index
    else
      loop (index + 1)
  in
  loop start

let find_char_from = fun ~char ~start source ->
  let rec loop index =
    if index >= String.length source then
      None
    else if Char.equal (String.get_unchecked source ~at:index) char then
      Some index
    else
      loop (index + 1)
  in
  loop start

let workspace_manifest_path = fun (workspace: Workspace_manifest.t) ->
  Path.(workspace.root / Path.v "riot.toml")

let relative_member_path = fun ~(workspace:Workspace_manifest.t) path ->
  let normalized_path = Path.normalize path in
  if Path.is_absolute normalized_path then
    Path.strip_prefix normalized_path ~prefix:(Path.normalize workspace.root)
    |> Result.map ~fn:Path.normalize
    |> Result.map_err ~fn:(fun _ -> "Package path must live under the workspace root")
  else
    Ok (Path.normalize normalized_path)

let add_workspace_member_to_source = fun ~member source ->
  let quoted_member = "\"" ^ member ^ "\"" in
  if String.contains source quoted_member then
    Ok source
  else
    let* workspace_index =
      match find_substring ~needle:"[workspace]" ~start:0 source with
      | Some index -> Ok index
      | None -> Error "Failed to find [workspace] section in riot.toml"
    in
    let* members_index =
      match find_substring ~needle:"members" ~start:workspace_index source with
      | Some index -> Ok index
      | None -> Error "Failed to find workspace members in riot.toml"
    in
    let* open_index =
      match find_char_from ~char:'[' ~start:members_index source with
      | Some index -> Ok index
      | None -> Error "Failed to parse workspace members in riot.toml"
    in
    let* close_index =
      match find_char_from ~char:']' ~start:(open_index + 1) source with
      | Some index -> Ok index
      | None -> Error "Failed to find the end of workspace members in riot.toml"
    in
    let before_members = String.sub source ~offset:0 ~len:(open_index + 1) in
    let members_body =
      String.sub source ~offset:(open_index + 1) ~len:(close_index - open_index - 1)
    in
    let after_members =
      String.sub source ~offset:close_index ~len:(String.length source - close_index)
    in
    let inserted_body =
      if String.is_empty (String.trim members_body) then
        "\n  " ^ quoted_member ^ ",\n"
      else if String.contains members_body "\n" then
        members_body ^ "  " ^ quoted_member ^ ",\n"
      else
        "\n  " ^ String.trim members_body ^ ",\n  " ^ quoted_member ^ ",\n"
    in
    Ok (before_members ^ inserted_body ^ after_members)

let add_workspace_member = fun ~(workspace:Workspace_manifest.t) ~path ->
  let* relative_path = relative_member_path ~workspace path in
  let manifest_path = workspace_manifest_path workspace in
  let* manifest_source =
    Fs.read_to_string manifest_path
    |> Result.map_err ~fn:IO.error_message
  in
  let* updated_source =
    add_workspace_member_to_source ~member:(Path.to_string relative_path) manifest_source in
  Fs.write updated_source manifest_path
  |> Result.map_err ~fn:(fun err -> "Failed to update workspace manifest: " ^ IO.error_message err)
