open Global
open Collections

type ctx = {
  test: Test_context.t;
  fixture_path: Path.t;
  fixture_relpath: Path.t;
  fixture_name: string;
}

type filter_result =
[
  `keep
  | `skip
]

let is_snapshot_artifact = fun path ->
  let basename = Path.basename path in
  String.ends_with ~suffix:".expected" basename || String.ends_with ~suffix:".expected.new" basename

let discover_fixture_paths = fun root ->
  match Fs.Walker.to_list ~roots:[ root ] ~sort:true ~include_directories:false () with
  | Error err -> Error err
  | Ok found ->
      Ok (
        found |> List.filter_map
          (fun (entry: Fs.Walker.entry) ->
            if not (is_snapshot_artifact entry.path) then
              (Some entry.path)
            else
              None) |> List.sort Path.compare
      )

let relpath = fun ~root path ->
  match Path.strip_prefix path ~prefix:root with
  | Ok relpath -> relpath
  | Error _ -> path

let fixture_name = fun relpath -> relpath |> Path.remove_extension |> Path.to_string

let keep_path = fun filter path ->
  match filter path with
  | `keep -> true
  | `skip -> false

let cases = fun ?(filter = fun _ -> `keep) ?(snapshot_path = fun _ -> None) () ~dir ~run ->
  let root =
    if Path.is_relative dir then
      match Env.current_dir () with
      | Ok cwd -> Path.join cwd dir
      | Error _ -> dir
    else
      dir
  in
  let fixtures =
    match discover_fixture_paths root with
    | Ok fixtures -> fixtures
    | Error err -> panic
      ("failed to discover fixtures under " ^ Path.to_string root ^ ": " ^ IO.error_message err)
  in
  fixtures |> List.filter (keep_path filter) |> List.map
    (fun fixture_path ->
      let fixture_relpath = relpath ~root fixture_path in
      let fixture_name = fixture_name fixture_relpath in
      Test_case.case (Path.to_string fixture_relpath)
        (fun test_ctx ->
          let fixture =
            Test_context.{
              path = fixture_path;
              relpath = fixture_relpath;
              name = fixture_name;
              snapshot_path = snapshot_path fixture_path
            } in
          run
            {
              test = Test_context.with_fixture test_ctx fixture;
              fixture_path;
              fixture_relpath;
              fixture_name
            }))
