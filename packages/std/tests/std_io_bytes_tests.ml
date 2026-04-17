open Std

let test_create_has_requested_length = fun _ctx ->
  let bytes = IO.Bytes.create ~size:4 in
  if Int.equal (IO.Bytes.length bytes) 4 then
    Ok ()
  else
    Error "IO.Bytes.create should allocate the requested length"

let test_get_returns_some_for_valid_index = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  match IO.Bytes.get bytes ~at:2 with
  | Some value when Char.equal value 'c' -> Ok ()
  | _ -> Error "IO.Bytes.get should return Some for valid indices"

let test_get_returns_none_for_invalid_index = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  match IO.Bytes.get bytes ~at:9 with
  | None -> Ok ()
  | Some _ -> Error "IO.Bytes.get should return None for invalid indices"

let test_get_unchecked_returns_the_byte_directly = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  if Char.equal (IO.Bytes.get_unchecked bytes ~at:1) 'b' then
    Ok ()
  else
    Error "IO.Bytes.get_unchecked should return the byte directly"

let test_set_mutates_valid_index = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  match IO.Bytes.set bytes ~at:1 ~char:'Z' with
  | Ok () when String.equal (IO.Bytes.to_string bytes) "aZcd" -> Ok ()
  | Ok () -> Error "IO.Bytes.set should mutate the selected byte"
  | Error _ -> Error "IO.Bytes.set should succeed for valid indices"

let test_set_rejects_invalid_index = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  match IO.Bytes.set bytes ~at:9 ~char:'Z' with
  | Error _ -> Ok ()
  | Ok () -> Error "IO.Bytes.set should reject invalid indices"

let test_set_unchecked_mutates_without_result = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  IO.Bytes.set_unchecked bytes ~at:0 ~char:'Z';
  if String.equal (IO.Bytes.to_string bytes) "Zbcd" then
    Ok ()
  else
    Error "IO.Bytes.set_unchecked should mutate the selected byte"

let test_blit_full_copy_copies_source_into_destination = fun _ctx ->
  let src = IO.Bytes.from_string "abcd" in
  let dst = IO.Bytes.create ~size:4 in
  match IO.Bytes.blit src ~src_offset:0 ~dst ~dst_offset:0 ~len:4 with
  | Ok () when String.equal (IO.Bytes.to_string dst) "abcd" -> Ok ()
  | Ok () -> Error "IO.Bytes.blit should copy the requested slice"
  | Error _ -> Error "IO.Bytes.blit should succeed for valid slices"

let test_blit_overlapping_right_shift_matches_bytes_semantics = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcdef" in
  match IO.Bytes.blit bytes ~src_offset:0 ~dst:bytes ~dst_offset:2 ~len:4 with
  | Ok () when String.equal (IO.Bytes.to_string bytes) "ababcd" -> Ok ()
  | Ok () -> Error "IO.Bytes.blit should handle overlapping right shifts"
  | Error _ -> Error "IO.Bytes.blit should succeed for valid overlapping slices"

let test_blit_overlapping_left_shift_matches_bytes_semantics = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcdef" in
  match IO.Bytes.blit bytes ~src_offset:2 ~dst:bytes ~dst_offset:0 ~len:4 with
  | Ok () when String.equal (IO.Bytes.to_string bytes) "cdefef" -> Ok ()
  | Ok () -> Error "IO.Bytes.blit should handle overlapping left shifts"
  | Error _ -> Error "IO.Bytes.blit should succeed for valid overlapping slices"

let test_blit_invalid_slice_returns_error = fun _ctx ->
  let src = IO.Bytes.from_string "abcd" in
  let dst = IO.Bytes.create ~size:4 in
  match IO.Bytes.blit src ~src_offset:3 ~dst ~dst_offset:0 ~len:4 with
  | Error _ -> Ok ()
  | Ok () -> Error "IO.Bytes.blit should reject invalid slices"

let test_blit_string_copies_exact_characters = fun _ctx ->
  let dst = IO.Bytes.create ~size:5 in
  IO.Bytes.blit_string "hello" ~src_offset:0 ~dst ~dst_offset:0 ~len:5;
  if String.equal (IO.Bytes.to_string dst) "hello" then
    Ok ()
  else
    Error "IO.Bytes.blit_string should copy the requested characters"

let test_fill_updates_only_requested_range = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcdef" in
  IO.Bytes.fill bytes ~offset:1 ~len:3 ~char:'Z';
  if String.equal (IO.Bytes.to_string bytes) "aZZZef" then
    Ok ()
  else
    Error "IO.Bytes.fill should update only the selected range"

let test_from_string_and_to_string_roundtrip = fun _ctx ->
  let bytes = IO.Bytes.from_string "hello" in
  if String.equal (IO.Bytes.to_string bytes) "hello" then
    Ok ()
  else
    Error "IO.Bytes.from_string/to_string should roundtrip the content"

let test_sub_returns_expected_slice = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcdef" in
  match IO.Bytes.sub bytes ~offset:2 ~len:3 with
  | Ok slice when String.equal (IO.Bytes.to_string slice) "cde" -> Ok ()
  | Ok _ -> Error "IO.Bytes.sub returned the wrong slice"
  | Error _ -> Error "IO.Bytes.sub should succeed for valid ranges"

let test_sub_rejects_invalid_range = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcdef" in
  match IO.Bytes.sub bytes ~offset:(-1) ~len:3 with
  | Error _ -> Ok ()
  | Ok _ -> Error "IO.Bytes.sub should reject invalid ranges"

let test_multiple_mutations_preserve_final_contents = fun _ctx ->
  let bytes = IO.Bytes.from_string "abcd" in
  IO.Bytes.set_unchecked bytes ~at:0 ~char:'X';
  IO.Bytes.set_unchecked bytes ~at:3 ~char:'Y';
  if String.equal (IO.Bytes.to_string bytes) "XbcY" then
    Ok ()
  else
    Error "IO.Bytes should reflect all mutations in order"

let tests = Test.[
  case "create allocates the requested length" test_create_has_requested_length;
  case "get returns Some for valid indices" test_get_returns_some_for_valid_index;
  case "get returns None for invalid indices" test_get_returns_none_for_invalid_index;
  case "get_unchecked returns the selected byte" test_get_unchecked_returns_the_byte_directly;
  case "set mutates valid indices" test_set_mutates_valid_index;
  case "set rejects invalid indices" test_set_rejects_invalid_index;
  case "set_unchecked mutates without returning a result" test_set_unchecked_mutates_without_result;
  case "blit copies full slices" test_blit_full_copy_copies_source_into_destination;
  case "blit handles overlapping right shifts" test_blit_overlapping_right_shift_matches_bytes_semantics;
  case "blit handles overlapping left shifts" test_blit_overlapping_left_shift_matches_bytes_semantics;
  case "blit rejects invalid slices" test_blit_invalid_slice_returns_error;
  case "blit_string copies exact characters" test_blit_string_copies_exact_characters;
  case "fill updates only the requested range" test_fill_updates_only_requested_range;
  case "from_string and to_string roundtrip" test_from_string_and_to_string_roundtrip;
  case "sub returns the expected slice" test_sub_returns_expected_slice;
  case "sub rejects invalid ranges" test_sub_rejects_invalid_range;
  case "multiple mutations preserve final contents" test_multiple_mutations_preserve_final_contents;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"io_bytes" ~tests ~args) ~args:Env.args ()
