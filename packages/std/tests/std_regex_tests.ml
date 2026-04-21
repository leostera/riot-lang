open Std
module Test = Std.Test

let tests = [ Test.case "regex DSL compiles and matches"
    (fun _ctx ->
      let regex = Regex.seq
        [
          Regex.start_of_text;
          Regex.literal "foo.";
          Regex.one_or_more (Regex.char_class [ Regex.Range ('0', '9') ]);
          Regex.end_of_text;
        ]
      |> Regex.compile
      |> Result.expect ~msg:"compile regex" in
      Test.assert_true (Regex.is_match regex "foo.42");
      Test.assert_false (Regex.is_match regex "fooX42");
      Ok ()); Test.case "regex from_string compiles directly"
    (fun _ctx ->
      let regex = Regex.from_string "^foo[0-9]+$" |> Result.expect ~msg:"compile regex" in
      Test.assert_true (Regex.is_match regex "foo42");
      Test.assert_false (Regex.is_match regex "bar42");
      Ok ()); Test.case "regex escapes literal metacharacters"
    (fun _ctx ->
      let regex = Regex.literal "foo.+(bar)" |> Regex.compile |> Result.expect ~msg:"compile regex" in
      Test.assert_true (Regex.is_match regex "foo.+(bar)");
      Test.assert_false (Regex.is_match regex "fooZZbar");
      Ok ()); ]

let main = fun ~args -> Test.Cli.main ~name:"std_regex_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
