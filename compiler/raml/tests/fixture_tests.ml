open Std

let ( let* ) = Result.and_then

let fixtures_dir = Path.v "compiler/raml/tests/fixtures"

let keep_source_fixture = fun path ->
  let path_components = String.split_on_char '/' (Path.to_string path) in
  if
    List.exists (String.equal "typ_lowering") path_components
    || List.exists (String.equal "corpus") path_components
  then
    `skip
  else
    match Path.extension path with
    | Some ".ml"
    | Some ".mli" -> `keep
    | _ -> `skip

let test_fixture = fun ~(ctx:Test.FixtureRunner.ctx) ->
  let* source = Result.map_error IO.error_message (Fs.read ctx.fixture_path) in
  let* unit_ = Raml.Source_unit.from_source ~relpath:ctx.fixture_relpath ~source in
  Test.Snapshot.assert_json ~ctx:ctx.test ~actual:(Raml.Source_unit.to_json unit_)

let () =
  Actors.run
    ~main:(fun ~args ->
      let tests =
        Test.FixtureRunner.cases
          ()
          ~dir:fixtures_dir
          ~filter:keep_source_fixture
          ~run:(fun ctx -> test_fixture ~ctx)
      in
      Test.Cli.main ~name:"raml:fixture_tests" ~tests ~args)
    ~args:Env.args
    ()
