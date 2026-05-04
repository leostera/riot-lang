open Std
open Std.Data

let compare_with_baseline = fun input actual ->
  let expected = Json.from_string input in
  match (expected, actual) with
  | (Ok left, Ok right) when left = right -> Ok ()
  | (Error left, Error right) when left = right -> Ok ()
  | (Ok left, Ok right) ->
      Error ("JsonStream parsed a different value. expected="
      ^ Json.to_string left
      ^ " actual="
      ^ Json.to_string right)
  | (Error left, Error right) ->
      Error ("JsonStream returned a different error. expected="
      ^ Json.error_to_string left
      ^ " actual="
      ^ Json.error_to_string right)
  | (Ok left, Error right) ->
      Error ("JsonStream failed when Json succeeded. expected="
      ^ Json.to_string left
      ^ " error="
      ^ Json.error_to_string right)
  | (Error left, Ok right) ->
      Error ("JsonStream succeeded when Json failed. error="
      ^ Json.error_to_string left
      ^ " actual="
      ^ Json.to_string right)

let test_from_string_matches_json = fun _ctx ->
  let cases = [
    "null";
    "true";
    "false";
    "-123";
    "3.14";
    "1.5e10";
    {|"hello\nworld"|};
    {|"\u0000\t\u001F"|};
    "[1, 2, 3]";
    {|{"name":"Alice","tags":["a","b"],"active":true}|};
    "  \n  {\"nested\": [1, {\"ok\": false}, null]}  \n  ";
  ]
  in
  List.fold_left
    cases
    ~init:(Ok ())
    ~fn:(fun acc input ->
      match acc with
      | Error _ as error -> error
      | Ok () -> compare_with_baseline input (JsonStream.from_string input))

let test_invalid_literal_reports_position_and_text = fun _ctx ->
  match JsonStream.from_string "tru" with
  | Error (Json.Invalid_literal { expected; position; found }) when String.equal expected "true"
  && Int.equal position 0
  && String.equal found "tru" -> Ok ()
  | Error error ->
      Error ("expected Invalid_literal for 'tru', got " ^ JsonStream.error_to_string error)
  | Ok value -> Error ("expected parsing to fail for 'tru', got " ^ Json.to_string value)

let test_extra_input_after_value_reports_position = fun _ctx ->
  match JsonStream.from_string "null null" with
  | Error (Json.Extra_input_after_value { position }) when Int.equal position 5 -> Ok ()
  | Error error ->
      Error ("expected Extra_input_after_value for trailing input, got "
      ^ JsonStream.error_to_string error)
  | Ok value -> Error ("expected parsing to fail for trailing input, got " ^ Json.to_string value)

let test_from_slice_matches_json = fun _ctx ->
  let input = {|{"users":[{"id":1,"name":"Alice"},{"id":2,"name":"Bob"}],"ok":true}|} in
  let slice =
    IO.IoVec.IoSlice.from_string input
    |> Result.expect ~msg:"slice creation failed"
  in
  compare_with_baseline input (JsonStream.from_slice slice)

let test_from_slice_matches_json_on_mixed_arrays = fun _ctx ->
  let input = "[1, 2, {\"name\": \"riot\"}, false]" in
  let slice =
    IO.IoVec.IoSlice.from_string input
    |> Result.expect ~msg:"slice creation failed"
  in
  compare_with_baseline input (JsonStream.from_slice slice)

let test_large_numeric_array_matches_json = fun _ctx ->
  let payload =
    "[" ^ (
      List.init ~count:5_000 ~fn:Int.to_string
      |> String.concat ","
    ) ^ "]"
  in
  compare_with_baseline payload (JsonStream.from_string payload)

let tests =
  Test.[
    case "from_string matches Json on representative inputs" test_from_string_matches_json;
    case "invalid literals report position and text" test_invalid_literal_reports_position_and_text;
    case
      "extra input after value reports the trailing position"
      test_extra_input_after_value_reports_position;
    case "from_slice matches Json on nested objects" test_from_slice_matches_json;
    case "from_slice matches Json on mixed arrays" test_from_slice_matches_json_on_mixed_arrays;
    case "large numeric arrays match Json" test_large_numeric_array_matches_json;
  ]

let main ~args = Test.Cli.main ~name:"json_stream" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
