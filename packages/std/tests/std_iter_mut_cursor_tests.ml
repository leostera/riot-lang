open Std

module MutCursor = Iter.MutCursor
module IoSlice = IO.Iovec.IoSlice

let is_digit = fun ch -> Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let test_create_sets_source_position_and_length_remaining = fun _ctx ->
  let cursor = MutCursor.create "hello" in
  if
    String.equal (IoSlice.to_string (MutCursor.source cursor)) "hello"
    && Int.equal (MutCursor.position cursor) 0
    && Int.equal (MutCursor.length_remaining cursor) 5
  then
    Ok ()
  else
    Error "MutCursor.create should start at position 0 with the full length remaining"

let test_peek_returns_the_first_character = fun _ctx ->
  match MutCursor.peek (MutCursor.create "hello") with
  | Some value when Char.equal value 'h' -> Ok ()
  | _ -> Error "MutCursor.peek should return the current character"

let test_advance_moves_position_by_one = fun _ctx ->
  let cursor = MutCursor.create "hello" in
  MutCursor.advance cursor;
  if Int.equal (MutCursor.position cursor) 1 && String.equal (IoSlice.to_string (MutCursor.remaining cursor)) "ello" then
    Ok ()
  else
    Error "MutCursor.advance should move the cursor by one position"

let test_advance_by_moves_within_bounds = fun _ctx ->
  let cursor = MutCursor.create "hello" in
  MutCursor.advance_by cursor 3;
  if Int.equal (MutCursor.position cursor) 3 && String.equal (IoSlice.to_string (MutCursor.remaining cursor)) "lo" then
    Ok ()
  else
    Error "MutCursor.advance_by should move within bounds"

let test_take_while_collects_prefix_and_advances = fun _ctx ->
  let cursor = MutCursor.create "123abc" in
  let taken = MutCursor.take_while cursor is_digit in
  if String.equal (IoSlice.to_string taken) "123" && String.equal (IoSlice.to_string (MutCursor.remaining cursor)) "abc" then
    Ok ()
  else
    Error "MutCursor.take_while should collect the matching prefix"

let test_take_until_stops_before_the_matching_character = fun _ctx ->
  let cursor = MutCursor.create "alpha:beta" in
  match MutCursor.take_until cursor (fun ch -> Char.equal ch ':') with
  | Some taken
    when String.equal (IoSlice.to_string taken) "alpha"
         && String.equal (IoSlice.to_string (MutCursor.remaining cursor)) ":beta" ->
      Ok ()
  | _ -> Error "MutCursor.take_until should stop before the matching character"

let test_take_n_returns_requested_length = fun _ctx ->
  let cursor = MutCursor.create "abcdef" in
  match MutCursor.take_n cursor 3 with
  | Some taken
    when String.equal (IoSlice.to_string taken) "abc"
         && String.equal (IoSlice.to_string (MutCursor.remaining cursor)) "def" ->
      Ok ()
  | _ -> Error "MutCursor.take_n should return the requested number of characters"

let test_remaining_is_empty_at_eof = fun _ctx ->
  let cursor = MutCursor.create "abc" in
  MutCursor.advance_by cursor 3;
  if String.equal (IoSlice.to_string (MutCursor.remaining cursor)) "" && MutCursor.is_eof cursor then
    Ok ()
  else
    Error "MutCursor.remaining should be empty at EOF"

let tests = Test.[
  case "create sets source position and remaining length" test_create_sets_source_position_and_length_remaining;
  case "peek returns the first character" test_peek_returns_the_first_character;
  case "advance moves forward by one character" test_advance_moves_position_by_one;
  case "advance_by moves within bounds" test_advance_by_moves_within_bounds;
  case "take_while collects prefixes" test_take_while_collects_prefix_and_advances;
  case "take_until stops before the matching character" test_take_until_stops_before_the_matching_character;
  case "take_n returns the requested number of characters" test_take_n_returns_requested_length;
  case "remaining is empty at EOF" test_remaining_is_empty_at_eof;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"iter_mut_cursor" ~tests ~args) ~args:Env.args ()
