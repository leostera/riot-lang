open Std
module IoVec = IO.IoVec
module Bytes = Kernel.Bytes

let test_create_allocates_requested_total_length = fun _ctx ->
  let iov = IoVec.create ~count:2 ~size:5 () |> Result.unwrap in
  if Int.equal (IoVec.length iov) 5 then
    Ok ()
  else
    Error "IoVec.create should allocate the requested total length"

let test_with_capacity_creates_single_segment = fun _ctx ->
  let iov = IoVec.with_capacity 4 |> Result.unwrap in
  if Int.equal (IoVec.length iov) 4 then
    Ok ()
  else
    Error "IoVec.with_capacity should create an iovec with the requested length"

let test_from_bytes_roundtrips_through_to_bytes = fun _ctx ->
  let bytes = Bytes.from_string "hello" in
  let iov = IoVec.from_bytes bytes |> Result.unwrap in
  if String.equal (Bytes.to_string (IoVec.to_bytes iov)) "hello" then
    Ok ()
  else
    Error "IoVec.from_bytes/to_bytes should roundtrip the content"

let test_from_string_roundtrips_through_to_string = fun _ctx ->
  let iov = IoVec.from_string "hello" |> Result.unwrap in
  if String.equal (IoVec.to_string iov) "hello" then
    Ok ()
  else
    Error "IoVec.from_string/to_string should roundtrip the content"

let test_from_bytes_array_concatenates_segments = fun _ctx ->
  let iov = IoVec.from_bytes_array [|Bytes.from_string "ab"; Bytes.from_string "cd"|] |> Result.unwrap in
  if String.equal (IoVec.to_string iov) "abcd" then
    Ok ()
  else
    Error "IoVec.from_bytes_array should concatenate segments in order"

let test_from_string_array_concatenates_segments = fun _ctx ->
  let iov = IoVec.from_string_array [|"ab"; "cd"; "ef"|] |> Result.unwrap in
  if String.equal (IoVec.to_string iov) "abcdef" then
    Ok ()
  else
    Error "IoVec.from_string_array should concatenate segments in order"

let test_for_each_visits_segments_in_insertion_order = fun _ctx ->
  let iov = IoVec.from_string_array [|"ab"; "c"; "def"|] |> Result.unwrap in
  let seen = Sync.Atomic.make [] in
  IoVec.for_each iov
    ~fn:(fun segment ->
      Sync.Atomic.set seen (IoVec.IoSlice.to_string segment :: Sync.Atomic.get seen));
  let segments = List.reverse (Sync.Atomic.get seen) in
  if segments = [ "ab"; "c"; "def" ] then
    Ok ()
  else
    Error "IoVec.for_each should visit segments in insertion order"

let test_sub_returns_prefix = fun _ctx ->
  let iov = IoVec.from_string "abcdef" |> Result.unwrap in
  if String.equal
      (
        IoVec.to_string
          (IoVec.sub iov ~len:3 |> Result.unwrap)
      )
      "abc" then
    Ok ()
  else
    Error "IoVec.sub should return the requested prefix"

let test_sub_can_slice_across_segment_boundaries = fun _ctx ->
  let iov = IoVec.from_string_array [|"ab"; "cd"; "ef"|] |> Result.unwrap in
  if String.equal
      (
        IoVec.to_string
          (IoVec.sub iov ~pos:1 ~len:4 |> Result.unwrap)
      )
      "bcde" then
    Ok ()
  else
    Error "IoVec.sub should slice across segment boundaries"

let test_sub_full_length_returns_all_bytes = fun _ctx ->
  let iov = IoVec.from_string_array [|"ab"; "cd"; "ef"|] |> Result.unwrap in
  let full = IoVec.sub iov ~pos:0 ~len:(IoVec.length iov) |> Result.unwrap in
  if String.equal (IoVec.to_string full) "abcdef" then
    Ok ()
  else
    Error "IoVec.sub should return all bytes for a full-length slice"

let tests =
  Test.[
    case "create allocates the requested total length" test_create_allocates_requested_total_length;
    case "with_capacity creates the requested length" test_with_capacity_creates_single_segment;
    case "from_bytes roundtrips through to_bytes" test_from_bytes_roundtrips_through_to_bytes;
    case "from_string roundtrips through to_string" test_from_string_roundtrips_through_to_string;
    case "from_bytes_array concatenates segments" test_from_bytes_array_concatenates_segments;
    case "from_string_array concatenates segments" test_from_string_array_concatenates_segments;
    case "for_each visits segments in insertion order" test_for_each_visits_segments_in_insertion_order;
    case "sub returns prefixes" test_sub_returns_prefix;
    case "sub slices across segment boundaries" test_sub_can_slice_across_segment_boundaries;
    case "sub full length returns all bytes" test_sub_full_length_returns_all_bytes;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"io_iovec" ~tests ~args) ~args:Env.args ()
