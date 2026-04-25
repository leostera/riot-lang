open Std
module Method = Std.Net.Http.Method
module IoSlice = IO.IoVec.IoSlice

let test_from_slice_standard = fun _ctx ->
  let slice = IoSlice.from_string "GET" |> Result.unwrap in
  match Method.from_slice slice with
  | Method.Get -> Ok ()
  | method_ -> Error ("Expected GET, got " ^ Method.to_string method_)

let test_from_slice_extension = fun _ctx ->
  let slice = IoSlice.from_string "PURGE" |> Result.unwrap in
  match Method.from_slice slice with
  | Method.Extension "PURGE" -> Ok ()
  | method_ -> Error ("Expected PURGE extension, got " ^ Method.to_string method_)

let tests =
  Test.[
    case "from_slice parses standard methods" test_from_slice_standard;
    case "from_slice preserves extensions" test_from_slice_extension;
  ]

let main ~args = Test.Cli.main ~name:"net_http_method" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
