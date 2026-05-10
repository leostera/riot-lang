open Std
open Markdown

let checksum = ref 0

let touch_parse_result = fun (result: Markdown.parse_result) ->
  let value = List.length result.blocks lxor List.length result.diagnostics in
  checksum := !checksum lxor value

let touch_string = fun text -> checksum := !checksum lxor String.length text

type commonmark_fixture = { markdown: string }

let json_field = fun fields name ->
  fields
  |> List.find ~fn:(fun (field_name, _) -> String.equal field_name name)
  |> Option.map ~fn:(fun (_, value) -> value)

let fixture_from_json = fun json ->
  match Data.Json.get_object json with
  | None -> None
  | Some fields ->
      match json_field fields "markdown" with
      | Some (Data.Json.String markdown) when not (String.is_empty markdown) -> Some { markdown }
      | Some _
      | None -> None

let locate_commonmark_fixture_file = fun () ->
  let current_dir =
    match Env.current_dir () with
    | Ok path -> path
    | Error _ -> Path.v "."
  in
  let roots =
    [
      Env.get Env.String ~var:"RIOT_WORKSPACE_ROOT"
      |> Option.map ~fn:Path.v;
      Some current_dir;
    ]
    |> List.filter_map ~fn:(fun root -> root)
  in
  let candidates =
    roots
    |> List.map
      ~fn:(fun root -> Path.join root (Path.v "packages/markdown/tests/spec_fixtures.json"))
  in
  List.find
    candidates
    ~fn:(fun path ->
      Fs.exists path
      |> Result.unwrap_or ~default:false)

let load_commonmark_fixtures = fun () ->
  match locate_commonmark_fixture_file () with
  | None -> []
  | Some path ->
      match Fs.read path with
      | Error _ -> []
      | Ok source ->
          match Data.Json.from_string source with
          | Error _ -> []
          | Ok json ->
              match Data.Json.get_array json with
              | None -> []
              | Some rows -> List.filter_map rows ~fn:fixture_from_json

let commonmark_fixtures = load_commonmark_fixtures ()

let gfm_sources = [
  "~~gone~~\n";
  "- [ ] todo\n- [x] done\n";
  "| a | b |\n| --- | ---: |\n| c | d |\n";
]

let bench_parse_commonmark_corpus = fun () ->
  List.for_each
    commonmark_fixtures
    ~fn:(fun fixture ->
      let parsed = Markdown.parse fixture.markdown in
      touch_parse_result parsed)

let bench_parse_render_commonmark_corpus = fun () ->
  List.for_each
    commonmark_fixtures
    ~fn:(fun fixture -> touch_string (Markdown.compile fixture.markdown))

let bench_parse_gfm_extensions = fun () ->
  List.for_each
    gfm_sources
    ~fn:(fun source ->
      let parsed = Markdown.parse_gfm source in
      touch_parse_result parsed)

let incremental_source = "# Title\n\nhello world\n\n- item\n"

let incremental_updated_source = "# Title\n\nhello markdown\n\n- item\n"

let incremental_edit = { Markdown.Document.start = 15; end_ = 20; text = "markdown" }

let incremental_document = Markdown.Document.parse incremental_source

let bench_incremental_simple_block_edit = fun () ->
  let updated = Markdown.Document.update incremental_document ~edit:incremental_edit in
  Markdown.Document.to_parse_result updated
  |> touch_parse_result

let bench_full_reparse_simple_block_edit = fun () ->
  Markdown.Document.parse incremental_updated_source
  |> Markdown.Document.to_parse_result
  |> touch_parse_result

let corpus_config: Bench.bench_config = { iterations = 8; warmup = 1 }

let small_config: Bench.bench_config = { iterations = 2_000; warmup = 100 }

let benchmarks =
  Bench.[
    with_config
      ~config:corpus_config
      "markdown parse commonmark fixture corpus"
      bench_parse_commonmark_corpus;
    with_config
      ~config:corpus_config
      "markdown parse+render commonmark fixture corpus"
      bench_parse_render_commonmark_corpus;
    with_config
      ~config:small_config
      "markdown parse gfm extension corpus"
      bench_parse_gfm_extensions;
    with_config
      ~config:small_config
      "markdown incremental update simple block"
      bench_incremental_simple_block_edit;
    with_config
      ~config:small_config
      "markdown full reparse simple block edit"
      bench_full_reparse_simple_block_edit;
  ]

let main ~args =
  let result = Bench.Cli.main ~name:"markdown parser" ~benchmarks ~args in
  if !checksum = Int.min_int then
    panic "unreachable markdown benchmark checksum";
  result

let () = Runtime.run ~main ~args:Env.args ()
