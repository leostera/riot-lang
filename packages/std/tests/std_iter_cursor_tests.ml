open Std
module Cursor = Iter.Cursor
module IoSlice = IO.IoVec.IoSlice

let is_digit = fun ch -> Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let test_create_sets_source_position_and_length_remaining = fun _ctx ->
  let cursor = Cursor.create "hello" in
  if
    String.equal (IoSlice.to_string (Cursor.source cursor)) "hello"
    && Int.equal (Cursor.position cursor) 0
    && Int.equal (Cursor.length_remaining cursor) 5
  then
    Ok ()
  else
    Error "Cursor.create should start at position 0 with the full length remaining"

let test_peek_returns_the_first_character = fun _ctx ->
  match Cursor.peek (Cursor.create "hello") with
  | Some value when Char.equal value 'h' -> Ok ()
  | _ -> Error "Cursor.peek should return the current character"

let test_peek_n_returns_future_characters_without_advancing = fun _ctx ->
  let cursor = Cursor.create "hello" in
  match Cursor.peek_n cursor 1 with
  | Some value when Char.equal value 'e' && Int.equal (Cursor.position cursor) 0 -> Ok ()
  | _ -> Error "Cursor.peek_n should look ahead without advancing"

let test_advance_moves_position_by_one = fun _ctx ->
  match Cursor.advance (Cursor.create "hello") with
  | Some cursor when Int.equal (Cursor.position cursor) 1
  && String.equal (IoSlice.to_string (Cursor.remaining cursor)) "ello" -> Ok ()
  | _ -> Error "Cursor.advance should move the cursor by one position"

let test_advance_at_eof_returns_none = fun _ctx ->
  match Cursor.advance (Cursor.create "") with
  | None -> Ok ()
  | Some _ -> Error "Cursor.advance should return None at EOF"

let test_advance_by_moves_within_bounds = fun _ctx ->
  match Cursor.advance_by (Cursor.create "hello") 3 with
  | Some cursor when Int.equal (Cursor.position cursor) 3
  && String.equal (IoSlice.to_string (Cursor.remaining cursor)) "lo" -> Ok ()
  | _ -> Error "Cursor.advance_by should move within bounds"

let test_advance_by_past_eof_returns_none = fun _ctx ->
  match Cursor.advance_by (Cursor.create "hello") 6 with
  | None -> Ok ()
  | Some _ -> Error "Cursor.advance_by should return None when advancing past EOF"

let test_take_while_collects_prefix_and_returns_rest = fun _ctx ->
  let taken, cursor = Cursor.take_while (Cursor.create "123abc") is_digit in
  if
    String.equal (IoSlice.to_string taken) "123"
    && String.equal (IoSlice.to_string (Cursor.remaining cursor)) "abc"
  then
    Ok ()
  else
    Error "Cursor.take_while should collect the matching prefix"

let test_skip_while_advances_without_returning_text = fun _ctx ->
  let cursor =
    Cursor.skip_while (Cursor.create "   abc")
      (fun ch ->
        Char.equal ch ' ')
  in
  if String.equal (IoSlice.to_string (Cursor.remaining cursor)) "abc" then
    Ok ()
  else
    Error "Cursor.skip_while should advance while the predicate holds"

let test_take_until_stops_before_the_matching_character = fun _ctx ->
  match
    Cursor.take_until (Cursor.create "alpha:beta")
      (fun ch ->
        Char.equal ch ':')
  with
  | Some (taken, cursor) when String.equal (IoSlice.to_string taken) "alpha"
  && String.equal (IoSlice.to_string (Cursor.remaining cursor)) ":beta" -> Ok ()
  | _ -> Error "Cursor.take_until should stop before the matching character"

let test_take_until_char_stops_before_the_matching_character = fun _ctx ->
  match Cursor.take_until_char (Cursor.create "alpha:beta") ':' with
  | Some (taken, cursor) when String.equal (IoSlice.to_string taken) "alpha"
  && String.equal (IoSlice.to_string (Cursor.remaining cursor)) ":beta" -> Ok ()
  | _ -> Error "Cursor.take_until_char should stop before the matching character"

let test_take_n_returns_requested_length = fun _ctx ->
  match Cursor.take_n (Cursor.create "abcdef") 3 with
  | Some (taken, cursor) when String.equal (IoSlice.to_string taken) "abc"
  && String.equal (IoSlice.to_string (Cursor.remaining cursor)) "def" -> Ok ()
  | _ -> Error "Cursor.take_n should return the requested number of characters"

let test_remaining_is_empty_at_eof = fun _ctx ->
  let cursor = Cursor.advance_by (Cursor.create "abc") 3 |> Option.unwrap in
  if String.equal (IoSlice.to_string (Cursor.remaining cursor)) "" && Cursor.is_eof cursor then
    Ok ()
  else
    Error "Cursor.remaining should be empty at EOF"

let tests =
  Test.[
    case "create sets source position and remaining length" test_create_sets_source_position_and_length_remaining;
    case "peek returns the first character" test_peek_returns_the_first_character;
    case "peek_n looks ahead without advancing" test_peek_n_returns_future_characters_without_advancing;
    case "advance moves forward by one character" test_advance_moves_position_by_one;
    case "advance returns None at EOF" test_advance_at_eof_returns_none;
    case "advance_by moves within bounds" test_advance_by_moves_within_bounds;
    case "advance_by returns None past EOF" test_advance_by_past_eof_returns_none;
    case "take_while collects prefixes" test_take_while_collects_prefix_and_returns_rest;
    case "skip_while advances without returning text" test_skip_while_advances_without_returning_text;
    case "take_until stops before the matching character" test_take_until_stops_before_the_matching_character;
    case "take_until_char stops before the matching character" test_take_until_char_stops_before_the_matching_character;
    case "take_n returns the requested number of characters" test_take_n_returns_requested_length;
    case "remaining is empty at EOF" test_remaining_is_empty_at_eof;
  ]

let main ~args = Test.Cli.main ~name:"iter_cursor" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
