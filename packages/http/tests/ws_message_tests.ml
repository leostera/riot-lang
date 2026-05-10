open Std

module Frame = Http.Ws.Frame
module Message = Http.Ws.Message

let make_state = fun ?max_message_size () ->
  match Message.create ?max_message_size () with
  | Ok state -> Ok state
  | Error error -> Error (Message.error_to_string error)

let handle = fun state frame ->
  match Message.handle_frame state frame with
  | Ok result -> Ok result
  | Error error -> Error (Message.error_to_string error)

let expect_handle_error = fun state frame expected ->
  match Message.handle_frame state frame with
  | Error error when error = expected -> Ok ()
  | Error error -> Error ("Wrong message error: " ^ Message.error_to_string error)
  | Ok _ -> Error "Expected WebSocket message error"

let text_fragment = fun ?(fin = false) payload -> Frame.text ~fin payload

let binary_fragment = fun ?(fin = false) payload -> Frame.binary ~fin payload

let continuation = fun ?(fin = true) payload -> Frame.continuation ~fin payload

let test_unfragmented_text_emits_message = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      match handle state (Frame.text "hello") with
      | Ok (_, Some (Message.DataMessage { opcode = Message.Text; payload = "hello" })) -> Ok ()
      | Ok _ -> Error "Expected text data message"
      | Error error -> Error error

let test_fragmented_text_emits_after_final_continuation = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      match handle state (text_fragment "hel") with
      | Error error -> Error error
      | Ok (state, Some _) -> Error "Fragment start emitted a message"
      | Ok (state, None) ->
          match handle state (continuation "lo") with
          | Ok (_, Some (Message.DataMessage { opcode = Message.Text; payload = "hello" })) -> Ok ()
          | Ok _ -> Error "Expected completed fragmented text message"
          | Error error -> Error error

let test_control_frame_allowed_during_fragmented_message = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      match handle state (text_fragment "hel") with
      | Error error -> Error error
      | Ok (state, Some _) -> Error "Fragment start emitted a message"
      | Ok (state, None) ->
          match handle state (Frame.ping ()) with
          | Error error -> Error error
          | Ok (state, Some (Message.ControlFrame { Frame.opcode = Frame.Ping; _ })) ->
              match handle state (continuation "lo") with
              | Ok (_, Some (Message.DataMessage { opcode = Message.Text; payload = "hello" })) ->
                  Ok ()
              | Ok _ -> Error "Expected text message after ping"
              | Error error -> Error error
          | Ok _ -> Error "Expected ping control event"

let test_continuation_without_fragment_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state -> expect_handle_error state (continuation "x") Message.ContinuationWithoutFragment

let test_data_frame_while_fragmented_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      match handle state (text_fragment "hel") with
      | Error error -> Error error
      | Ok (state, Some _) -> Error "Fragment start emitted a message"
      | Ok (state, None) ->
          expect_handle_error
            state
            (Frame.text "second")
            (Message.DataFrameWhileFragmented { opcode = Message.Text })

let test_invalid_fragmented_text_utf8_fails_on_completion = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      match handle state (text_fragment "\xc0") with
      | Error error -> Error error
      | Ok (state, Some _) -> Error "Fragment start emitted a message"
      | Ok (state, None) ->
          expect_handle_error
            state
            (continuation "\x80")
            (Message.InvalidTextMessageUtf8 { payload_length = 2 })

let test_message_size_limit_spans_fragments = fun _ctx ->
  match make_state ~max_message_size:3 () with
  | Error error -> Error error
  | Ok state ->
      match handle state (text_fragment "ab") with
      | Error error -> Error error
      | Ok (state, Some _) -> Error "Fragment start emitted a message"
      | Ok (state, None) ->
          expect_handle_error
            state
            (continuation "cd")
            (Message.MessagePayloadTooLarge { payload_length = 4; max_message_size = 3 })

let test_binary_fragmented_message_emits_binary = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      match handle state (binary_fragment "\x00\x01") with
      | Error error -> Error error
      | Ok (state, Some _) -> Error "Fragment start emitted a message"
      | Ok (state, None) ->
          match handle state (continuation "\x02") with
          | Ok (_, Some (
            Message.DataMessage { opcode = Message.Binary; payload = "\x00\x01\x02" }
          )) ->
              Ok ()
          | Ok _ -> Error "Expected completed fragmented binary message"
          | Error error -> Error error

let test_fragmented_control_frame_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      expect_handle_error
        state
        Frame.{ (ping ()) with fin = false }
        (Message.FragmentedControlFrame { opcode = Message.Ping })

let test_oversized_control_frame_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      let payload = String.make ~len:126 ~char:'x' in
      expect_handle_error
        state
        Frame.{ (ping ()) with payload }
        (Message.ControlFramePayloadTooLarge { opcode = Message.Ping; payload_length = 126 })

let test_one_byte_close_payload_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      expect_handle_error
        state
        (Frame.close ~payload:"\x03" ())
        (Message.InvalidClosePayload (Frame.ClosePayloadTooShort { payload_length = 1 }))

let test_invalid_close_code_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      expect_handle_error
        state
        (Frame.close ~payload:"\x03\xed" ())
        (Message.InvalidClosePayload (Frame.InvalidCloseCode { code = 1_005 }))

let test_invalid_close_reason_utf8_fails = fun _ctx ->
  match make_state () with
  | Error error -> Error error
  | Ok state ->
      expect_handle_error
        state
        (Frame.close ~payload:"\x03\xe8\xc0" ())
        (Message.InvalidClosePayload (Frame.InvalidCloseReasonUtf8 { reason_length = 1 }))

let tests =
  Test.[
    case "unfragmented text emits message" test_unfragmented_text_emits_message;
    case
      "fragmented text emits after final continuation"
      test_fragmented_text_emits_after_final_continuation;
    case
      "control frame allowed during fragmented message"
      test_control_frame_allowed_during_fragmented_message;
    case "continuation without fragment fails" test_continuation_without_fragment_fails;
    case "data frame while fragmented fails" test_data_frame_while_fragmented_fails;
    case
      "invalid fragmented text utf8 fails on completion"
      test_invalid_fragmented_text_utf8_fails_on_completion;
    case "message size limit spans fragments" test_message_size_limit_spans_fragments;
    case "binary fragmented message emits binary" test_binary_fragmented_message_emits_binary;
    case "fragmented control frame fails" test_fragmented_control_frame_fails;
    case "oversized control frame fails" test_oversized_control_frame_fails;
    case "one byte close payload fails" test_one_byte_close_payload_fails;
    case "invalid close code fails" test_invalid_close_code_fails;
    case "invalid close reason utf8 fails" test_invalid_close_reason_utf8_fails;
  ]

let main ~args = Test.Cli.main ~name:"http:ws-message" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
