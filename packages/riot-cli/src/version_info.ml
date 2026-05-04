open Std

let ( let* ) value fn = Result.and_then value ~fn

type t = {
  release_id: string;
  build_sha: string;
  notes_url: string option;
  compare_url: string option;
  issues_url: string option;
}

let release_label = fun t -> t.release_id

let metadata_path = fun () ->
  let* riot_home = Riot_model.Riot_dirs.user_riot_dir () in
  Ok Path.(riot_home / Path.v "release.json")

let json_of_metadata = fun t ->
  Data.Json.Object [
    ("release_id", Data.Json.String t.release_id);
    ("build_sha", Data.Json.String t.build_sha);
    ("notes_url", match t.notes_url with
    | Some value -> Data.Json.String value
    | None -> Data.Json.Null);
    ("compare_url", match t.compare_url with
    | Some value -> Data.Json.String value
    | None -> Data.Json.Null);
    ("issues_url", match t.issues_url with
    | Some value -> Data.Json.String value
    | None -> Data.Json.Null);
  ]

let string_field = fun fields name ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | Some (_, Data.Json.String value) -> Ok value
  | _ -> Error ("missing or invalid '" ^ name ^ "' field")

let optional_string_field = fun fields name ->
  match List.find fields ~fn:(fun (field_name, _) -> String.equal field_name name) with
  | None
  | Some (_, Data.Json.Null) -> Ok None
  | Some (_, Data.Json.String value) -> Ok (Some value)
  | Some _ -> Error ("invalid '" ^ name ^ "' field")

let metadata_of_json = fun __tmp1 ->
  match __tmp1 with
  | Data.Json.Object fields ->
      let* release_id = string_field fields "release_id" in
      let* build_sha = string_field fields "build_sha" in
      let* notes_url = optional_string_field fields "notes_url" in
      let* compare_url = optional_string_field fields "compare_url" in
      let* issues_url = optional_string_field fields "issues_url" in
      Ok {
        release_id;
        build_sha;
        notes_url;
        compare_url;
        issues_url;
      }
  | _ -> Error "release metadata must be a JSON object"

let from_json_string = fun content ->
  let* json =
    Data.Json.from_string content
    |> Result.map_err ~fn:Data.Json.error_to_string
  in
  metadata_of_json json

let from_version_string = fun version ->
  let prefix = "riot " in
  let build_marker = " (build " in
  let suffix = ")" in
  let prefix_len = String.length prefix in
  let marker_len = String.length build_marker in
  let suffix_len = String.length suffix in
  let find_marker haystack needle =
    let haystack_len = String.length haystack in
    let needle_len = String.length needle in
    let rec loop index =
      if index + needle_len > haystack_len then
        None
      else if String.equal (String.sub haystack ~offset:index ~len:needle_len) needle then
        Some index
      else
        loop (index + 1)
    in
    loop 0
  in
  if String.length version <= prefix_len + marker_len + suffix_len then
    None
  else if not (String.starts_with ~prefix version) then
    None
  else
    let payload_len = String.length version - prefix_len in
    let payload = String.sub version ~offset:prefix_len ~len:payload_len in
    match find_marker payload build_marker with
    | None -> None
    | Some marker_pos ->
        let release_id =
          String.sub payload ~offset:0 ~len:marker_pos
          |> String.trim
        in
        let build_section_len = String.length payload - marker_pos in
        let build_section = String.sub payload ~offset:marker_pos ~len:build_section_len in
        if not (String.starts_with ~prefix:build_marker build_section) then
          None
        else if not (String.ends_with ~suffix build_section) then
          None
        else
          let build_sha_len = build_section_len - marker_len - suffix_len in
          let build_sha = String.sub build_section ~offset:marker_len ~len:build_sha_len in
          Some {
            release_id;
            build_sha;
            notes_url = None;
            compare_url = None;
            issues_url = None;
          }

let from_path = fun path ->
  let* content =
    Fs.read path
    |> Result.map_err ~fn:IO.error_message
  in
  from_json_string content

let write_path = fun ~path t ->
  let* () =
    Fs.create_dir_all (Path.dirname path)
    |> Result.map_err ~fn:IO.error_message
  in
  let content = Data.Json.to_string_pretty (json_of_metadata t) ^ "\n" in
  Fs.write content path
  |> Result.map_err ~fn:IO.error_message

let same_identity = fun left right ->
  String.equal left.release_id right.release_id && String.equal left.build_sha right.build_sha

let read_installed = fun () ->
  match metadata_path () with
  | Error _ -> None
  | Ok path -> (
      match Fs.exists path with
      | Ok false
      | Error _ -> None
      | Ok true ->
          from_path path
          |> Result.to_option
    )

let write_installed = fun t ->
  let* path = metadata_path () in
  write_path ~path t

let version_string_of = fun metadata ->
  "riot " ^ metadata.release_id ^ " (build " ^ metadata.build_sha ^ ")"

let version_string = fun () ->
  match read_installed () with
  | Some metadata -> version_string_of metadata
  | None -> "riot dev (build unknown)"

let agent_string = fun () ->
  match read_installed () with
  | Some metadata -> "riot-cli@" ^ metadata.release_id
  | None -> "riot-cli@dev"
