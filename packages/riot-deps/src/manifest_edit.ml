open Std

type section =
  | Runtime
  | Build
  | Dev

let section_name = function
  | Runtime -> "dependencies"
  | Build -> "build-dependencies"
  | Dev -> "dev-dependencies"

type error =
  | ReadFailed of { path: Path.t; error: IO.error }
  | WriteFailed of { path: Path.t; error: IO.error }
  | TomlParseFailed of { path: Path.t; error: Std.Data.Toml.error }
  | InvalidDependencyName of {
      path: Path.t;
      dependency: string;
      error: Riot_model.Package_name.error
    }
  | DependencySectionMustBeTable of { path: Path.t; section: string }
  | ManifestMustBeTable of { path: Path.t }

let error_message = function
  | ReadFailed { path; error } -> "failed to read manifest '"
  ^ Path.to_string path
  ^ "': "
  ^ IO.error_message error
  | WriteFailed { path; error } -> "failed to write manifest '"
  ^ Path.to_string path
  ^ "': "
  ^ IO.error_message error
  | TomlParseFailed { path; error } -> "failed to parse manifest '"
  ^ Path.to_string path
  ^ "': "
  ^ Std.Data.Toml.error_to_string error
  | InvalidDependencyName { path; dependency; error } -> "manifest '"
  ^ Path.to_string path
  ^ "' has invalid dependency name '"
  ^ dependency
  ^ "': "
  ^ Riot_model.Package_name.error_message error
  | DependencySectionMustBeTable { path; section } -> "manifest '"
  ^ Path.to_string path
  ^ "' section ["
  ^ section
  ^ "] must be a table"
  | ManifestMustBeTable { path } -> "manifest '" ^ Path.to_string path ^ "' root must be a TOML table"

let quoted = fun value -> Std.Data.Toml.to_string (Std.Data.Toml.String value)

let render_dependency_table = fun name fields ->
  let rendered_fields =
    List.map fields ~fn:(fun (field, value) -> field ^ " = " ^ value)
  in
  name ^ " = { " ^ String.concat ", " rendered_fields ^ " }"

let render_dependency = fun (dep: Riot_model.Package.dependency) ->
  let name = Riot_model.Package_name.to_string dep.name in
  match dep.source with
  | { workspace=true; _ } ->
      name ^ " = { workspace = true }"
  | {
    path=None;
    source_locator=None;
    ref_=None;
    version=Some requirement;
    _
  } ->
      name ^ " = " ^ quoted (Std.Version.requirement_to_string requirement)
  | {
    path=None;
    source_locator=None;
    ref_=None;
    version=None;
    _
  } ->
      name ^ " = " ^ quoted "*"
  | {
    path;
    source_locator;
    ref_;
    version;
    _
  } ->
      let fields = [] in
      let fields =
        match path with
        | Some path -> ("path", quoted (Path.to_string path)) :: fields
        | None -> fields
      in
      let fields =
        match source_locator with
        | Some source_locator -> ("source", quoted source_locator) :: fields
        | None -> fields
      in
      let fields =
        match ref_ with
        | Some ref_ -> ("ref", quoted ref_) :: fields
        | None -> fields
      in
      let fields =
        match version with
        | Some requirement -> ("version", quoted (Std.Version.requirement_to_string requirement))
        :: fields
        | None -> fields
      in
      render_dependency_table name (List.reverse fields)

let render_section_lines = fun ~section dependencies ->
  let header = "[" ^ section_name section ^ "]" in
  let body = List.map dependencies ~fn:render_dependency in
  header :: body

let is_section_header = fun line ->
  let trimmed = String.trim line in
  match String.length trimmed with
  | len when len >= 3 -> Char.equal (String.get_unchecked trimmed ~at:0) '['
  && Char.equal (String.get_unchecked trimmed ~at:(len - 1)) ']'
  | _ -> false

let replace_section_lines = fun ~source ~section dependencies ->
  let header = "[" ^ section_name section ^ "]" in
  let replacement = render_section_lines ~section dependencies in
  let lines = String.split ~by:"\n" source in
  let len = List.length lines in
  let rec line_at index = function
    | [] -> None
    | line :: _ when index = 0 -> Some line
    | _ :: rest -> line_at (index - 1) rest
  in
  let rec find_header index =
    if index >= len then
      None
    else if Option.is_some_and (line_at index lines)
        ~fn:(fun line ->
          String.equal (String.trim line) header) then
      Some index
    else
      find_header (index + 1)
  in
  let list_slice start_ stop =
    let rec loop acc index =
      if index < start_ then
        acc
      else
        match line_at index lines with
        | Some line -> loop (line :: acc) (index - 1)
        | None -> acc
    in
    if stop <= start_ then
      []
    else
      loop [] (stop - 1)
  in
  match find_header 0 with
  | Some start_index ->
      let rec find_end index =
        if index >= len then
          len
        else if
          index > start_index && Option.is_some_and (line_at index lines) ~fn:is_section_header
        then
          index
        else
          find_end (index + 1)
      in
      let end_index = find_end (start_index + 1) in
      String.concat "\n" (list_slice 0 start_index @ replacement @ list_slice end_index len)
  | None ->
      let existing = lines in
      let needs_blank =
        match List.reverse existing with
        | [] -> false
        | last :: _ -> not (String.equal (String.trim last) "")
      in
      let prefix =
        if needs_blank then
          existing @ [ "" ]
        else
          existing
      in
      String.concat "\n" (prefix @ replacement)

let update_dependency_section = fun ~manifest_path ~section ~dependencies ->
  match Fs.read_to_string manifest_path with
  | Error err -> Error (ReadFailed { path = manifest_path; error = err })
  | Ok source ->
      let updated = replace_section_lines ~source ~section dependencies in
      match Fs.write updated manifest_path with
      | Ok () -> Ok ()
      | Error err -> Error (WriteFailed { path = manifest_path; error = err })
