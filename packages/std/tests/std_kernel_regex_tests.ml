open Std
module Test = Std.Test

let tests = [ Test.case "kernel regex compiles and matches"
    (fun _ctx ->
      let regex = Kernel.Regex.compile "foo[0-9]+" |> Result.expect ~msg:"compile regex" in
      Test.assert_true (Kernel.Regex.is_match regex "prefix foo42 suffix");
      Test.assert_false (Kernel.Regex.is_match regex "prefix bar suffix");
      Ok ()); Test.case "kernel regex exposes first match spans"
    (fun _ctx ->
      let regex = Kernel.Regex.compile "foo[0-9]+" |> Result.expect ~msg:"compile regex" in
      match Kernel.Regex.find regex "prefix foo42 suffix" with
      | None -> Error "expected first match"
      | Some { start; stop } ->
          Test.assert_equal ~expected:7 ~actual:start;
          Test.assert_equal ~expected:12 ~actual:stop;
          Ok ()); Test.case "kernel regex reports compile errors with offsets"
    (fun _ctx ->
      match Kernel.Regex.compile "(" with
      | Ok _ -> Error "expected invalid regex"
      | Error { message; offset } ->
          Test.assert_true (String.length message > 0);
          Test.assert_true (Option.is_some offset);
          Ok ()); ]

let main ~args = Test.Cli.main ~name:"std_kernel_regex_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
