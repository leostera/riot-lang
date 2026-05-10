open Std

type fixture = {
  markdown: string;
  html: string;
  example: int option;
  section: string option;
}

let normalize_newlines = fun source ->
  let has_char subject char =
    let rec loop index =
      if index >= String.length subject then
        false
      else if String.get_unchecked subject ~at:index = char then
        true
      else
        loop (index + 1)
    in
    loop 0
  in
  if not (has_char source '\r') then
    source
  else
    let buffer = IO.Buffer.create ~size:(String.length source) in
    String.for_each
      source
      ~fn:(fun char ->
        if not (Char.equal char '\r') then
          IO.Buffer.add_char buffer char
        else
          ());
  IO.Buffer.contents buffer

let line_field = fun fields name ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | (candidate, value) :: rest ->
        if String.equal candidate name then
          Some value
        else
          loop rest
  in
  loop fields

let get_string_field = fun fields name ->
  match line_field fields name with
  | Some value -> (
      match Data.Json.get_string value with
      | Some text -> Some text
      | None -> None
    )
  | None -> None

let get_int_field = fun fields name ->
  match line_field fields name with
  | Some value -> (
      match Data.Json.get_int value with
      | Some value -> Some value
      | None -> None
    )
  | None -> None

let parse_fixture_entry = fun json ->
  match Data.Json.get_object json with
  | None -> None
  | Some fields ->
      let markdown =
        match get_string_field fields "markdown" with
        | Some value -> value
        | None -> ""
      in
      let html =
        match get_string_field fields "html" with
        | Some value -> value
        | None -> ""
      in
      if String.is_empty markdown || String.is_empty html then
        None
      else
        Some {
          markdown = normalize_newlines markdown;
          html;
          example =
            (
              match get_int_field fields "example" with
              | Some value -> Some value
              | None -> None
            );
          section =
            (
              match get_string_field fields "section" with
              | Some value -> Some value
              | None -> None
            );
        }

let parse_fixtures_json = fun source ->
  match Data.Json.from_string source with
  | Error _ -> []
  | Ok json ->
      match Data.Json.get_array json with
      | None -> []
      | Some rows ->
          rows
          |> List.filter_map ~fn:parse_fixture_entry

let derive_workspace_root = fun path ->
  let segments =
    Path.components path
    |> List.map ~fn:Path.to_string
  in
  let rec take_prefix index values acc =
    match (index, values) with
    | (0, _) -> List.reverse acc
    | (_, []) -> List.reverse acc
    | (_, value :: rest) -> take_prefix (index - 1) rest (value :: acc)
  in
  let rec find_build_index index values =
    match values with
    | [] -> None
    | head :: tail ->
        if String.equal head "_build" then
          Some index
        else
          find_build_index (index + 1) tail
  in
  let join_path_segments segments =
    match segments with
    | [] -> "."
    | head :: _ when String.equal head "/" -> String.concat "/" segments
    | segments -> String.concat "/" segments
  in
  match find_build_index 0 segments with
  | None -> None
  | Some 0 -> None
  | Some index ->
      let prefix = take_prefix index segments [] in
      match prefix with
      | []
      | [ "." ] -> None
      | _ -> Some (Path.v (join_path_segments prefix))

let ancestry = fun start ->
  let rec loop count path acc =
    if count <= 0 then
      acc
    else
      match Path.parent path with
      | None -> path :: acc
      | Some next ->
          let deduped = path :: acc in
          if List.contains deduped ~value:next then
            deduped
          else
            loop (count - 1) next deduped
  in
  loop 12 start []

let rec dedupe = fun items ->
  match items with
  | [] -> []
  | head :: tail ->
      let tail' = List.filter tail ~fn:(fun path -> not (Path.equal path head)) in
      head :: dedupe tail'

let locate_fixture_path = fun () ->
  let workspace =
    match Env.get Env.String ~var:"RIOT_WORKSPACE_ROOT" with
    | Some root -> Some (Path.v root)
    | None -> None
  in
  let current_dir =
    match Env.current_dir () with
    | Ok path -> path
    | Error _ -> Path.v "."
  in
  let executable =
    let args =
      Env.args
      |> Array.from_list
    in
    let relative_executable =
      if Array.length args > 0 then
        Path.v (Array.get_unchecked args ~at:0)
      else
        Path.v "spec_fixtures_tests"
    in
    if Path.is_absolute relative_executable then
      relative_executable
    else
      Path.join current_dir relative_executable
  in
  let executable_root = derive_workspace_root executable in
  let root_candidates =
    let roots =
      [ workspace; executable_root; Some current_dir ]
      |> List.filter_map ~fn:(fun value -> value)
    in
    dedupe (List.concat (List.map roots ~fn:ancestry))
  in
  let file_candidates =
    root_candidates
    |> List.map
      ~fn:(fun root -> [
        Path.join root (Path.v "packages/markdown/tests/spec_fixtures.json");
        Path.join root (Path.v "packages/markdown/tests/spec_fixtures.json");
        Path.join root (Path.v "markdown/tests/spec_fixtures.json");
        Path.join root (Path.v "markdown/tests/spec_fixtures.json");
        Path.join root (Path.v "tests/spec_fixtures.json");
        Path.join root (Path.v "spec_fixtures.json");
      ])
    |> List.concat
  in
  let candidates =
    (((((file_candidates @ [ Path.v "packages/markdown/tests/spec_fixtures.json" ])
    @ [ Path.v "packages/markdown/tests/spec_fixtures.json" ])
    @ [ Path.v "markdown/tests/spec_fixtures.json" ])
    @ [ Path.v "markdown/tests/spec_fixtures.json" ])
    @ [ Path.v "tests/spec_fixtures.json" ])
    @ [ Path.v "spec_fixtures.json" ]
  in
  let candidates = dedupe candidates in
  let rec pick = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | head :: rest ->
        let exists = Fs.exists head in
        match exists with
        | Ok value ->
            if value then
              Some head
            else
              pick rest
        | Error _ -> pick rest
  in
  pick candidates

let load_spec_fixtures = fun () ->
  match locate_fixture_path () with
  | None -> []
  | Some path ->
      match Fs.read path with
      | Error _ -> []
      | Ok source -> parse_fixtures_json (normalize_newlines source)

let spec_fixture_cache: fixture list = load_spec_fixtures ()

let all_spec_fixtures = fun () -> spec_fixture_cache
