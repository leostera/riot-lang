open Global

(** Shared fixture discovery for [Std.Test] suites.

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
          then `keep
          else `skip)
        ~run
    ```

    The callback receives both the ordinary test context and fixture-specific
    metadata, allowing it to reuse [Std.Test.Snapshot] without inventing a
    package-local harness.
*)
type ctx = {
  test: Test_context.t;
  fixture_path: Path.t;
  fixture_relpath: string;
  fixture_name: string;
}
type filter_result =
  [ `keep
  | `skip ]
val cases:
  ?filter:(Path.t -> filter_result) ->
  unit ->
  dir:Path.t ->
  run:(ctx -> (unit, string) result) ->
  Test_case.t list
