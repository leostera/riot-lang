open Std
module Test = Std.Test
module Kernel = Kernel_new

let test_bool_to_string_uses_stable_lowercase_literals = fun _ctx ->
  if
    Kernel.String.equal (Kernel.Bool.to_string Kernel.Bool.true_) "true"
    && Kernel.String.equal (Kernel.Bool.to_string Kernel.Bool.false_) "false"
  then
    Ok ()
  else
    Error "expected Bool.to_string to use stable lowercase literals"

let test_char_of_int_checks_bounds = fun _ctx ->
  match (Kernel.Char.of_int 65, Kernel.Char.of_int (-1), Kernel.Char.of_int 256) with
  | (Some value, None, None) when Kernel.Char.to_int value = 65 -> Ok ()
  | _ -> Error "expected Char.of_int to accept only byte-sized values"

let test_array_init_builds_in_index_order = fun _ctx ->
  let seen = Kernel.Array.make 4 (-1) in
  let next = ref 0 in
  let built =
    Kernel.Array.init 4
      (fun index ->
        Kernel.Array.set seen !next index;
        next := !next + 1;
        index * 2)
  in
  if
    !next = 4
    && Kernel.Array.get seen 0 = 0
    && Kernel.Array.get seen 1 = 1
    && Kernel.Array.get seen 2 = 2
    && Kernel.Array.get seen 3 = 3
    && Kernel.Array.get built 0 = 0
    && Kernel.Array.get built 1 = 2
    && Kernel.Array.get built 2 = 4
    && Kernel.Array.get built 3 = 6
  then
    Ok ()
  else
    Error "expected Array.init to visit each index once from left to right"

let test_option_map_leaves_none_unforced = fun _ctx ->
  let called = ref false in
  let value =
    Kernel.Option.map
      (fun _ ->
        called := true;
        1)
      None
  in
  if not !called && Kernel.Option.is_none value && Kernel.Option.unwrap_or value ~default:3 = 3 then
    Ok ()
  else
    Error "expected Option.map to leave None untouched and avoid calling its mapper"

let test_result_and_then_short_circuits_errors = fun _ctx ->
  let called = ref false in
  let value =
    Kernel.Result.and_then (Kernel.Result.Error "boom")
      (fun _ ->
        called := true;
        Kernel.Result.Ok 1)
  in
  match value with
  | Kernel.Result.Error "boom" when not !called -> Ok ()
  | _ -> Error "expected Result.and_then to leave Error untouched and skip the next step"

let test_bool_not_flips_both_branches = fun _ctx ->
  if
    Kernel.Bool.not Kernel.Bool.true_ = Kernel.Bool.false_
    && Kernel.Bool.not Kernel.Bool.false_ = Kernel.Bool.true_
  then
    Ok ()
  else
    Error "expected Bool.not to invert both boolean branches"

let test_char_unsafe_of_int_roundtrips_byte_boundaries = fun _ctx ->
  let zero = Kernel.Char.unsafe_of_int 0 in
  let max_byte = Kernel.Char.unsafe_of_int 255 in
  if Kernel.Char.to_int zero = 0 && Kernel.Char.to_int max_byte = 255 then
    Ok ()
  else
    Error "expected Char.unsafe_of_int to preserve byte-sized boundaries"

let test_int_to_string_matches_runtime_for_negative_values = fun _ctx ->
  let samples = [ (-1); (-42); min_int ] in
  let all_match =
    List.for_all (fun sample -> Kernel.Int.to_string sample = string_of_int sample) samples
  in
  if all_match then
    Ok ()
  else
    Error "expected Int.to_string to match the runtime decimal rendering for negative values"

let test_float_equal_and_compare_match_runtime_semantics = fun _ctx ->
  let nan = 0.0 /. 0.0 in
  let negative_zero = (-0.0) in
  let positive_zero = 0.0 in
  if
    Kernel.Float.equal nan nan = (( = ) nan nan)
    && Kernel.Float.equal negative_zero positive_zero = (( = ) negative_zero positive_zero)
    && Kernel.Float.compare nan nan = compare nan nan
    && Kernel.Float.compare nan positive_zero = compare nan positive_zero
    && Kernel.Float.compare negative_zero positive_zero = compare negative_zero positive_zero
  then
    Ok ()
  else
    Error "expected Kernel.Float to preserve the raw runtime equality and ordering semantics"

let test_array_make_aliases_mutable_payloads = fun _ctx ->
  let shared = Kernel.Bytes.of_string "aa" in
  let values = Kernel.Array.make 3 shared in
  Kernel.Bytes.set (Kernel.Array.get values 1) 0 'b';
  if
    Kernel.Bytes.to_string (Kernel.Array.get values 0) = "ba"
    && Kernel.Bytes.to_string (Kernel.Array.get values 2) = "ba"
  then
    Ok ()
  else
    Error "expected Array.make to alias the repeated mutable payload"

let test_option_none_helpers_are_exact_complements = fun _ctx ->
  let value = None in
  if
    not (Kernel.Option.is_some value)
    && Kernel.Option.is_none value
    && Kernel.Option.unwrap_or value ~default:7 = 7
  then
    Ok ()
  else
    Error "expected Option None helpers to stay complementary and use the fallback"

let test_string_make_fills_every_slot = fun _ctx ->
  let value = Kernel.String.make 4 'x' in
  if
    Kernel.String.length value = 4
    && Kernel.String.get value 0 = 'x'
    && Kernel.String.get value 1 = 'x'
    && Kernel.String.get value 2 = 'x'
    && Kernel.String.get value 3 = 'x'
  then
    Ok ()
  else
    Error "expected String.make to fill every character slot"

let test_string_append_preserves_embedded_nul_bytes = fun _ctx ->
  let value = Kernel.String.append "a\000" "b" in
  if
    Kernel.String.length value = 3
    && Kernel.String.get value 0 = 'a'
    && Kernel.String.get value 1 = '\000'
    && Kernel.String.get value 2 = 'b'
  then
    Ok ()
  else
    Error "expected String.append to preserve embedded nul bytes"

let test_bytes_fill_only_touches_requested_window = fun _ctx ->
  let value = Kernel.Bytes.of_string "abcdef" in
  Kernel.Bytes.fill value 2 2 'X';
  if Kernel.Bytes.to_string value = "abXXef" then
    Ok ()
  else
    Error "expected Bytes.fill to leave bytes outside the requested window unchanged"

let test_bytes_blit_handles_overlapping_ranges = fun _ctx ->
  let value = Kernel.Bytes.of_string "abcdef" in
  Kernel.Bytes.blit value 0 value 2 4;
  if Kernel.Bytes.to_string value = "ababcd" then
    Ok ()
  else
    Error "expected Bytes.blit to behave like memmove on overlapping ranges"

let test_bytes_sub_zero_length_is_empty = fun _ctx ->
  let value = Kernel.Bytes.sub (Kernel.Bytes.of_string "riot") 2 0 in
  if Kernel.Bytes.length value = 0 then
    Ok ()
  else
    Error "expected Bytes.sub with len=0 to return empty bytes"

let test_bytes_sub_string_zero_length_is_empty = fun _ctx ->
  if Kernel.Bytes.sub_string (Kernel.Bytes.of_string "riot") 2 0 = "" then
    Ok ()
  else
    Error "expected Bytes.sub_string with len=0 to return an empty string"

let test_path_join_preserves_root_sanity = fun _ctx ->
  if Kernel.Path.to_string (Kernel.Path.join "/" "tmp") = "/tmp" then
    Ok ()
  else
    Error "expected Path.join to keep the root separator sane"

let test_path_join_does_not_duplicate_separators = fun _ctx ->
  if Kernel.Path.to_string (Kernel.Path.join "a" "/b") = "a/b" then
    Ok ()
  else
    Error "expected Path.join to avoid duplicate separators when the right side already starts with one"

let test_iovec_with_capacity_matches_create_count_one = fun _ctx ->
  let left = Kernel.IO.Iovec.with_capacity 7 in
  let right = Kernel.IO.Iovec.create ~count:1 ~size:7 () in
  let count_segments iov =
    let seen = ref 0 in
    Kernel.IO.Iovec.iter (fun _ -> seen := !seen + 1) iov;
    !seen
  in
  if
    Kernel.IO.Iovec.length left = Kernel.IO.Iovec.length right
    && count_segments left = 1
    && count_segments right = 1
  then
    Ok ()
  else
    Error "expected Iovec.with_capacity to match a single-segment create"

let test_iovec_create_distributes_remainder_deterministically = fun _ctx ->
  let iov = Kernel.IO.Iovec.create ~count:3 ~size:5 () in
  let lengths = ref [] in
  Kernel.IO.Iovec.iter (fun segment -> lengths := segment.length :: !lengths) iov;
  if List.rev !lengths = [ 2; 2; 1 ] then
    Ok ()
  else
    Error "expected Iovec.create to distribute remainder bytes from left to right"

let test_iovec_of_bytes_array_aliases_source_buffers = fun _ctx ->
  let left = Kernel.Bytes.of_string "ri" in
  let right = Kernel.Bytes.of_string "ot" in
  let iov = Kernel.IO.Iovec.of_bytes_array [|left; right|] in
  Kernel.Bytes.set left 0 'R';
  if Kernel.IO.Iovec.into_string iov = "Riot" then
    Ok ()
  else
    Error "expected Iovec.of_bytes_array to alias the original source buffers"

let test_iovec_into_bytes_returns_a_fresh_copy = fun _ctx ->
  let source = Kernel.Bytes.of_string "riot" in
  let iov = Kernel.IO.Iovec.of_bytes_array [|source|] in
  let flattened = Kernel.IO.Iovec.into_bytes iov in
  Kernel.Bytes.set source 0 'R';
  if Kernel.Bytes.to_string flattened = "riot" then
    Ok ()
  else
    Error "expected Iovec.into_bytes to return a fresh stable copy"

let test_iovec_iter_reports_left_to_right_segment_metadata = fun _ctx ->
  let first = Kernel.Bytes.of_string "ab" in
  let second = Kernel.Bytes.of_string "" in
  let third = Kernel.Bytes.of_string "cde" in
  let iov = Kernel.IO.Iovec.of_bytes_array [|first; second; third|] in
  let seen = ref [] in
  Kernel.IO.Iovec.iter
    (fun segment ->
      seen := (segment.offset, segment.length, Kernel.Bytes.length segment.buffer) :: !seen)
    iov;
  if List.rev !seen = [ (0, 2, 2); (0, 0, 0); (0, 3, 3) ] then
    Ok ()
  else
    Error "expected Iovec.iter to preserve segment order and metadata"

let test_iovec_sub_zero_length_is_empty = fun _ctx ->
  let iov = Kernel.IO.Iovec.of_string_array [|"hello"; " "; "riot"|] in
  let sub = Kernel.IO.Iovec.sub ~pos:3 ~len:0 iov in
  if Kernel.IO.Iovec.length sub = 0 then
    Ok ()
  else
    Error "expected Iovec.sub with len=0 to return an empty iovec"

let tests = [
  Test.case "Bool.to_string uses stable lowercase literals" test_bool_to_string_uses_stable_lowercase_literals;
  Test.case "Char.of_int checks bounds" test_char_of_int_checks_bounds;
  Test.case "Array.init builds in index order" test_array_init_builds_in_index_order;
  Test.case "Option.map leaves None unforced" test_option_map_leaves_none_unforced;
  Test.case "Result.and_then short-circuits errors" test_result_and_then_short_circuits_errors;
  Test.case "Bool.not flips both branches" test_bool_not_flips_both_branches;
  Test.case "Char.unsafe_of_int roundtrips byte boundaries" test_char_unsafe_of_int_roundtrips_byte_boundaries;
  Test.case "Int.to_string matches runtime rendering for negatives and min_int" test_int_to_string_matches_runtime_for_negative_values;
  Test.case "Float.equal and Float.compare match runtime semantics" test_float_equal_and_compare_match_runtime_semantics;
  Test.case "Array.make aliases mutable payloads" test_array_make_aliases_mutable_payloads;
  Test.case "Option None helpers stay complementary" test_option_none_helpers_are_exact_complements;
  Test.case "String.make fills every slot" test_string_make_fills_every_slot;
  Test.case "String.append preserves embedded nul bytes" test_string_append_preserves_embedded_nul_bytes;
  Test.case "Bytes.fill only touches the requested window" test_bytes_fill_only_touches_requested_window;
  Test.case "Bytes.blit handles overlapping ranges" test_bytes_blit_handles_overlapping_ranges;
  Test.case "Bytes.sub with len=0 is empty" test_bytes_sub_zero_length_is_empty;
  Test.case "Bytes.sub_string with len=0 is empty" test_bytes_sub_string_zero_length_is_empty;
  Test.case "Path.join keeps the root sane" test_path_join_preserves_root_sanity;
  Test.case "Path.join avoids duplicate separators" test_path_join_does_not_duplicate_separators;
  Test.case "Iovec.with_capacity matches create count one" test_iovec_with_capacity_matches_create_count_one;
  Test.case "Iovec.create distributes remainder deterministically" test_iovec_create_distributes_remainder_deterministically;
  Test.case "Iovec.of_bytes_array aliases its source buffers" test_iovec_of_bytes_array_aliases_source_buffers;
  Test.case "Iovec.into_bytes returns a fresh copy" test_iovec_into_bytes_returns_a_fresh_copy;
  Test.case "Iovec.iter preserves segment order and metadata" test_iovec_iter_reports_left_to_right_segment_metadata;
  Test.case "Iovec.sub with len=0 is empty" test_iovec_sub_zero_length_is_empty;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_foundation_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
