open Std
module Version = Std.Net.Http.Version
module IoSlice = IO.IoVec.IoSlice

let test_from_slice_standard = fun _ctx ->
  let slice = IoSlice.from_string "HTTP/1.1" |> Result.unwrap in
  match Version.from_slice slice with
  | Ok Version.Http11 -> Ok ()
  | Ok version -> Error ("Expected HTTP/1.1, got " ^ Version.to_string version)
  | Error `InvalidVersion -> Error "Expected HTTP/1.1 to parse"

let test_from_slice_invalid = fun _ctx ->
  let slice = IoSlice.from_string "HTTP/9.9" |> Result.unwrap in
  match Version.from_slice slice with
  | Error `InvalidVersion -> Ok ()
  | Ok version -> Error ("Expected invalid version, got " ^ Version.to_string version)

let tests =
  Test.[
    case "from_slice parses supported versions" test_from_slice_standard;
    case "from_slice rejects invalid versions" test_from_slice_invalid;
  ]

let () =
  Runtime.run
    ~main:(fun ~args -> Test.Cli.main ~name:"net_http_version" ~tests ~args ())
    ~args:Env.args
    ()
