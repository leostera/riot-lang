open Std
module Test = Std.Test

let tests = [ Test.case "glob star stays within a path segment"
    (fun _ctx ->
      let glob = Glob.create [ "src/*.ml" ] |> Result.expect ~msg:"create glob" in
      Test.assert_true
        (Glob.matches glob ~str:"src/main.ml" |> Result.unwrap_or ~default:false);
      Test.assert_false
        (Glob.matches glob ~str:"src/lib/main.ml" |> Result.unwrap_or ~default:false);
      Ok ()); Test.case "glob recursive wildcard crosses separators"
    (fun _ctx ->
      let glob = Glob.create [ "./**" ] |> Result.expect ~msg:"create glob" in
      Test.assert_true
        (Glob.matches glob ~str:"./foo/bar/baz.ml" |> Result.unwrap_or ~default:false);
      Test.assert_true
        (Glob.matches glob ~str:"./" |> Result.unwrap_or ~default:false);
      Ok ()); Test.case "glob character classes and single wildcards compile"
    (fun _ctx ->
      let glob = Glob.create [ "test/[ab]?.ml" ] |> Result.expect ~msg:"create glob" in
      Test.assert_true
        (Glob.matches glob ~str:"test/a1.ml" |> Result.unwrap_or ~default:false);
      Test.assert_true
        (Glob.matches glob ~str:"test/bx.ml" |> Result.unwrap_or ~default:false);
      Test.assert_false
        (Glob.matches glob ~str:"test/c1.ml" |> Result.unwrap_or ~default:false);
      Test.assert_false
        (Glob.matches glob ~str:"test/a12.ml" |> Result.unwrap_or ~default:false);
      Ok ()); Test.case "glob parse errors report offsets"
    (fun _ctx ->
      match Glob.create [ "foo[" ] with
      | Ok _ ->
          Error "expected invalid glob"
      | Error (Glob.Invalid_glob { message; offset; _ }) ->
          Test.assert_true (String.length message > 0);
          Test.assert_equal ~expected:(Some 3) ~actual:offset;
          Ok ()
      | Error _ ->
          Error "expected invalid glob"); Test.case "glob create compiles many patterns into one matcher"
    (fun _ctx ->
      let glob = Glob.create [ "./*"; "../**/woo" ] |> Result.expect ~msg:"create glob set" in
      Test.assert_true
        (Glob.matches glob ~str:"./main" |> Result.unwrap_or ~default:false);
      Test.assert_true
        (Glob.matches glob ~str:"../a/b/woo" |> Result.unwrap_or ~default:false);
      Test.assert_false
        (Glob.matches glob ~str:"./a/b" |> Result.unwrap_or ~default:false);
      Ok ()); Test.case "glob doublestar slash matches zero or more directories"
    (fun _ctx ->
      let glob = Glob.create [ "**/vendor" ] |> Result.expect ~msg:"create glob" in
      Test.assert_true
        (Glob.matches glob ~str:"vendor" |> Result.unwrap_or ~default:false);
      Test.assert_true
        (Glob.matches glob ~str:"src/vendor" |> Result.unwrap_or ~default:false);
      Test.assert_true
        (Glob.matches glob ~str:"a/b/vendor" |> Result.unwrap_or ~default:false);
      Test.assert_false
        (Glob.matches glob ~str:"vendored" |> Result.unwrap_or ~default:false);
      Ok ()); ]

let main = fun ~args -> Test.Cli.main ~name:"std_glob_tests" ~tests ~args

let () = Runtime.run ~main ~args:Env.args ()
