open Std

module Test = Std.Test
module Body = Std.Net.Http.Body
module Request = Std.Net.Http.Request
module Response = Std.Net.Http.Response
module Status = Std.Net.Http.Status
module Uri = Std.Net.Uri
module IoSlice = IO.IoVec.IoSlice

let test_body_from_string = fun _ctx ->
  let body = Body.from_string "hello" in
  if Body.length body != 5 then
    Error "expected Body.length to report the string size"
  else if Option.is_some (Body.to_slice_opt body) then
    Error "expected string-backed bodies to stay owned strings"
  else if not (String.equal (Body.to_string body) "hello") then
    Error "expected Body.to_string to preserve string-backed bodies"
  else
    Ok ()

let test_body_from_slice = fun _ctx ->
  let slice =
    IoSlice.from_string "riot body"
    |> Result.unwrap
  in
  let body = Body.from_slice slice in
  match Body.to_slice_opt body with
  | None -> Error "expected slice-backed bodies to expose their borrowed slice"
  | Some borrowed ->
      if not (String.equal (IoSlice.to_string borrowed) "riot body") then
        Error "expected borrowed body slice contents to match the source slice"
      else if not (String.equal (Body.to_string body) "riot body") then
        Error "expected Body.to_string to materialize borrowed slices"
      else
        Ok ()

let test_request_with_body_slice = fun _ctx ->
  let uri =
    Uri.from_string "/upload"
    |> Result.unwrap
  in
  let slice =
    IoSlice.from_string "payload"
    |> Result.unwrap
  in
  let request =
    Request.create Std.Net.Http.Method.Post uri
    |> fun request -> Request.with_body_slice request slice
  in
  match Request.body request with
  | None -> Error "expected Request.with_body_slice to attach a body"
  | Some body ->
      if Request.body_string request != Some "payload" then
        Error "expected Request.body_string to materialize the borrowed body"
      else
        match Body.to_slice_opt body with
        | None -> Error "expected Request.body to preserve the borrowed slice representation"
        | Some borrowed ->
            if not (String.equal (IoSlice.to_string borrowed) "payload") then
              Error "expected Request.body borrowed slice contents to match"
            else
              Ok ()

let test_response_builder_body_data = fun _ctx ->
  let body = Body.from_string {|{"ok":true}|} in
  let response =
    Response.Builder.create Status.Ok
    |> fun builder ->
      Response.Builder.body_data builder body
      |> Response.Builder.build
  in
  match Response.body response with
  | None -> Error "expected Response.Builder.body_data to attach a body"
  | Some attached ->
      if not (String.equal (Body.to_string attached) {|{"ok":true}|}) then
        Error "expected Response.Builder.body_data to preserve the body contents"
      else
        Ok ()

let tests = [
  Test.case "Std.Net.Http.Body wraps owned strings" test_body_from_string;
  Test.case "Std.Net.Http.Body wraps borrowed slices" test_body_from_slice;
  Test.case "Std.Net.Http.Request preserves slice-backed bodies" test_request_with_body_slice;
  Test.case "Std.Net.Http.Response builder accepts body data" test_response_builder_body_data;
]

let main ~args = Test.Cli.main ~name:"std_net_http_body_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
