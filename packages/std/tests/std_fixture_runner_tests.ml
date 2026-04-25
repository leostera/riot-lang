open Std

let tests =
  Test.FixtureRunner.cases () ~dir:(Path.v "packages/std/tests/fixtures/snapshot_fixture_runner")
    ~filter:(fun path ->
      if String.ends_with ~suffix:".txt" (Path.basename path) then
        `keep
      else
        `skip)
    ~run:(fun ctx ->
      let actual = Fs.read ctx.fixture_path |> Result.expect ~msg:"read fixture" in
      Test.Snapshot.assert_text ~ctx:ctx.test ~actual)

let main ~args = Test.Cli.main ~name:"std_fixture_runner_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
