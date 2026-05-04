open Std

module Test = Std.Test
module Status = Std.Net.Http.Status

let test_status_equal_matches_codes = fun _ctx ->
  Test.assert_true (Status.equal Status.Ok (Status.from_int 200));
  Test.assert_false (Status.equal Status.Ok Status.NotFound);
  Ok ()

let test_status_equal_handles_extension_codes = fun _ctx ->
  Test.assert_true (Status.equal (Status.Extension 599) (Status.from_int 599));
  Test.assert_false (Status.equal (Status.Extension 599) (Status.Extension 598));
  Ok ()

let tests =
  Test.[
    case "Status.equal compares standard status codes" test_status_equal_matches_codes;
    case "Status.equal compares extension status codes" test_status_equal_handles_extension_codes;
  ]

let main ~args = Test.Cli.main ~name:"std_net_http_status_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
