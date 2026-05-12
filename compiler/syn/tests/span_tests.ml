open Std

let span start end_ = Syn.Span.make ~start ~end_

let assert_true ~msg value =
  if value then
    Ok ()
  else
    Error msg

let assert_false ~msg value = assert_true ~msg (not value)

let test_width _ctx =
  if Int.equal (Syn.Span.width (span 10 18)) 8 then
    Ok ()
  else
    Error "expected span width to be end minus start"

let test_contains_span _ctx =
  let outer = span 10 20 in
  let inner = span 12 18 in
  let touching_cursor = span 20 20 in
  let outside = span 19 21 in
  match assert_true ~msg:"expected outer span to contain inner span" (Syn.Span.contains outer inner) with
  | Error _ as error -> error
  | Ok () ->
      match assert_true
        ~msg:"expected outer span to contain end-boundary cursor"
        (Syn.Span.contains outer touching_cursor) with
      | Error _ as error -> error
      | Ok () ->
          assert_false
            ~msg:"expected outer span not to contain overlapping outside span"
            (Syn.Span.contains outer outside)

let test_compare_uses_span_length _ctx =
  match Syn.Span.compare (span 10 12) (span 50 55) with
  | Order.LT -> (
      match Syn.Span.compare (span 10 15) (span 50 55) with
      | Order.EQ -> Ok ()
      | _ -> Error "expected same-width spans to compare equal"
    )
  | _ -> Error "expected shorter span to compare before longer span"

let test_relative_order_helpers _ctx =
  let left = span 1 3 in
  let right = span 5 8 in
  match assert_true ~msg:"expected left to start before right" (Syn.Span.starts_before left right) with
  | Error _ as error -> error
  | Ok () ->
      match assert_true ~msg:"expected left to end before right" (Syn.Span.ends_before left right) with
      | Error _ as error -> error
      | Ok () ->
          match assert_true
            ~msg:"expected right to start after left"
            (Syn.Span.starts_after right left) with
          | Error _ as error -> error
          | Ok () ->
              assert_true ~msg:"expected right to end after left" (Syn.Span.ends_after right left)

let tests =
  Test.[
    case "span query width" test_width;
    case "span query contains span" test_contains_span;
    case "span query compare uses length" test_compare_uses_span_length;
    case "span query relative order helpers" test_relative_order_helpers;
  ]

let main ~args = Test.Cli.main ~name:"syn:span" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
