open Std
open Riot_model

let ( let* ) = Result.and_then

let out = eprintln

let command =
  let open ArgParser in
    let open Arg in command "doc"
    |> about "Generate package documentation"
    |> args
      [
        option "package" |> short 'p' |> long "package" |> help "Generate docs for a single package";
        flag "all" |> long "all" |> help "Generate docs for all workspace packages";
        flag "release" |> long "release" |> help "Generate release docs into _build/doc/<package>/<version>";
        option "output" |> long "output" |> short 'o' |> help "Override documentation output directory";
        flag "force" |> long "force" |> help "Ignore cache and regenerate docs";
        flag "no-cache" |> long "no-cache" |> help "Disable docs cache read/write";
      ]

let build_request = fun ~workspace matches ->
  ({
      workspace;
      package_name = ArgParser.get_one matches "package";
      all = ArgParser.get_flag matches "all";
      release = ArgParser.get_flag matches "release";
      output_root = Option.map Path.v (ArgParser.get_one matches "output");
      force = ArgParser.get_flag matches "force";
      no_cache = ArgParser.get_flag matches "no-cache";
    }: Riot_doc.request)

let display_path = fun ~workspace_root path ->
  match Path.strip_prefix path ~prefix:workspace_root with
  | Ok rel -> "./" ^ Path.to_string rel
  | Error _ -> Path.to_string path

let write_doc_event = fun ~workspace_root (event: Riot_doc.event) ->
  match event with
  | Riot_doc.PackageGenerationStarted _ -> ()
  | Riot_doc.PackageGenerationCompleted summary ->
      if not summary.cache_hit then
        out
          ("   \027[1;32mGenerated\027[0m "
          ^ summary.package
          ^ "@"
          ^ summary.version
          ^ " -> "
          ^ display_path ~workspace_root summary.output_dir)

let run = fun ~workspace matches ->
  let request = build_request ~workspace matches in
  let* _summaries = Riot_doc.run ~on_event:(write_doc_event ~workspace_root:workspace.root) request in
  Ok ()
