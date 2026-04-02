open Std

type section =
  | Runtime
  | Build
  | Dev

let section_name = function
  | Runtime -> "dependencies"
  | Build -> "build-dependencies"
  | Dev -> "dev-dependencies"

let quoted = fun value -> Std.Data.Toml.to_string (Std.Data.Toml.String value)

let render_dependency = fun (dep: Riot_model.Package.dependency) ->
  let name = dep.name in
  match dep.source with
  | { workspace=true; _ } -> name ^ " = { workspace = true }"
  | { path=None; version=Some requirement; _ } -> name
  ^ " = "
  ^ quoted (Std.Version.requirement_to_string requirement)
  | { path=None; version=None; _ } -> name ^ " = " ^ quoted "*"
  | { path=Some path; version=None; _ } -> name ^ " = { path = " ^ quoted (Path.to_string path) ^ " }"
  | { path=Some path; version=Some requirement; _ } -> name
  ^ " = { path = "
  ^ quoted (Path.to_string path)
  ^ ", version = "
  ^ quoted (Std.Version.requirement_to_string requirement)
  ^ " }"

let render_section_lines = fun ~section dependencies ->
  let header = "[" ^ section_name section ^ "]" in
  let body = List.map render_dependency dependencies in
  header :: body

let is_section_header = fun line ->
  let trimmed = String.trim line in
  match String.length trimmed with
  | len when len >= 3 -> Char.equal trimmed.[0] '[' && Char.equal trimmed.[len - 1] ']'
  | _ -> false

let replace_section_lines = fun ~source ~section dependencies ->
  let header = "[" ^ section_name section ^ "]" in
  let replacement = render_section_lines ~section dependencies in
  let lines = String.split_on_char '\n' source in
  let len = List.length lines in
  let rec line_at index = function
    | [] -> None
    | line :: _ when index = 0 -> Some line
    | _ :: rest -> line_at (index - 1) rest
  in
  let rec find_header index =
    if index >= len then
      None
    else if Option.is_some_and
        (fun line ->
          String.equal (String.trim line) header)
        (line_at index lines) then
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
        else if index > start_index && Option.is_some_and is_section_header (line_at index lines) then
          index
        else
          find_end (index + 1)
      in
      let end_index = find_end (start_index + 1) in
      String.concat "\n" (list_slice 0 start_index @ replacement @ list_slice end_index len)
  | None ->
      let existing = lines in
      let needs_blank =
        match List.rev existing with
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
  | Error err -> Error ("failed to read manifest '"
  ^ Path.to_string manifest_path
  ^ "': "
  ^ IO.error_message err)
  | Ok source ->
      let updated = replace_section_lines ~source ~section dependencies in
      match Fs.write updated manifest_path with
      | Ok () -> Ok ()
      | Error err -> Error ("failed to write manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
