open Global

(**
   Shared fixture discovery for [Std.Test] suites.

   [cases () ~dir ~run] recursively discovers fixture inputs under [dir], skips
   adjacent snapshot artifacts such as [.expected] and [.expected.new], and
   turns each remaining file into a regular [Std.Test.case].

   Use [filter] to narrow discovery when the fixture directory also contains
   helper files or several fixture families:

   ```ocaml
   let tests =
     Test.FixtureRunner.cases ()
       ~dir:(Std.Path.v "packages/riot-fix/tests")
       ~filter:(fun path ->
         let name = Std.Path.basename path in
         if Std.String.ends_with ~suffix:".ml" name
            && Std.String.length name >= 4
            && Std.Char.is_digit name.[0]
         then Keep
         else Skip)
       ~run
   ```

   The callback receives both the ordinary test context and fixture-specific
   metadata, allowing it to reuse [Std.Test.Snapshot] without inventing a
   package-local harness.

   Use [snapshot_path] when a fixture family already stores approved snapshots
   with a package-specific filename convention:

   ```ocaml
   let append_snapshot_suffix path suffix =
     Std.Path.to_string path ^ suffix
     |> Std.Path.from_string
     |> Result.expect ~msg:"snapshot path should stay valid UTF-8"

   let tests =
     Test.FixtureRunner.cases ()
       ~dir:(Std.Path.v "packages/syn/tests/fixtures")
       ~snapshot_path:(fun path ->
         Some (append_snapshot_suffix path ".expected_lossless.json"))
       ~filter
       ~run
   ```
*)
type ctx = {
  test: Test_context.t;
  fixture_path: Path.t;
  fixture_relpath: Path.t;
  fixture_name: string;
}
type filter_result =
  | Keep
  | Skip

val cases:
  ?filter:(Path.t -> filter_result) ->
  ?snapshot_path:(Path.t -> Path.t option) ->
  unit ->
  dir:Path.t ->
  run:(ctx -> (unit, string) result) ->
  Test_case.t list
