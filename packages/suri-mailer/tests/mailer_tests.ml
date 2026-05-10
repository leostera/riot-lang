open Std
open Result.Syntax

module Test = Std.Test
module Mailer = Suri_mailer
module Testing = Suri.Testing
module Response = Suri.Response

let expect = fun condition message ->
  if condition then
    Ok ()
  else
    Error message

let contains = String.contains

let expect_contains = fun body needle ->
  expect
    (contains body needle)
    ("expected body to contain: " ^ needle ^ "\n\n" ^ body)

let expect_delivered = fun result ->
  match result with
  | Ok () -> Ok ()
  | Error error -> Error (Mailer.Delivery.error_to_string error)

let expect_response = fun result ->
  match result with
  | Ok response -> Ok response
  | Error error -> Error (Testing.response_error_to_string error)

let expect_status = fun status response ->
  match Testing.Expect.status status response with
  | Ok () -> Ok ()
  | Error error ->
      Error (Testing.Expect.error_to_string error ^ "; body: " ^ Response.(response.body))

let app_with_mailer = fun supervisor ->
  Suri.Middleware.[ router Suri.Middleware.Router.[ forward "/__mailbox" (Mailer.routes supervisor); ] ]

let test_render_multipart = fun _ctx ->
  let message =
    Mailer.Message.make
      ~from:"KaraokeCrowd <no-reply@example.test>"
      ~to_:[ "singer@example.test" ]
      ~subject:"Welcome"
      ~text:"Welcome text"
      ~html:"<p>Welcome html</p>"
      ()
  in
  let rendered = Mailer.Message.render message in
  let* () = expect_contains rendered "Subject: Welcome" in
  let* () = expect_contains rendered "From: KaraokeCrowd <no-reply@example.test>" in
  let* () = expect_contains rendered "To: singer@example.test" in
  let* () = expect_contains rendered "Content-Type: multipart/alternative" in
  let* () = expect_contains rendered "Welcome text" in
  expect_contains rendered "<p>Welcome html</p>"

let test_render_sanitizes_headers = fun _ctx ->
  let message =
    Mailer.Message.make
      ~to_:[ "singer@example.test" ]
      ~subject:"Hello\r\nBcc: attacker@example.test"
      ~text:"Body"
      ()
  in
  let rendered = Mailer.Message.render message in
  let* () = expect_contains rendered "Subject: Hello  Bcc: attacker@example.test" in
  expect
    (not (contains rendered "\r\nBcc: attacker@example.test"))
    "expected injected Bcc header to be flattened"

let test_test_delivery_records_messages = fun _ctx ->
  let store = Mailer.TestDelivery.create_store () in
  let delivery = Mailer.TestDelivery.delivery store in
  let message =
    Mailer.Message.make ~to_:[ "singer@example.test" ] ~subject:"Recorded" ~text:"Body" ()
  in
  let* () = expect_delivered (Mailer.deliver_now delivery message) in
  match Mailer.TestDelivery.deliveries store with
  | [ delivered ] -> expect (delivered.subject = "Recorded") "expected recorded subject"
  | _ -> Error "expected exactly one delivered message"

let test_outbox_writes_eml = fun _ctx ->
  let dir = "/tmp/suri-mailer-outbox-tests" in
  let message =
    Mailer.Message.make
      ~to_:[ "singer@example.test" ]
      ~subject:"Outbox Link"
      ~text:"Open https://example.test/api/auth/verify?token=abc"
      ()
  in
  match Mailer.Delivery.deliver_to_outbox ~dir message with
  | Error error -> Error (Mailer.Delivery.error_to_string error)
  | Ok path ->
      match Fs.read path with
      | Error error -> Error ("failed to read outbox email: " ^ IO.error_message error)
      | Ok body ->
          let* () = expect_contains body "To: singer@example.test" in
          let* () = expect_contains body "Subject: Outbox Link" in
          expect_contains body "https://example.test/api/auth/verify?token=abc"

let test_invalid_message = fun _ctx ->
  let delivery = Mailer.Delivery.outbox ~dir:"/tmp/suri-mailer-outbox-tests" () in
  let message = Mailer.Message.make ~to_:[] ~subject:"No recipient" ~text:"Body" () in
  match Mailer.deliver_now delivery message with
  | Error (Mailer.Delivery.InvalidMessage _) -> Ok ()
  | Ok () -> Error "expected invalid-message error"
  | Error error ->
      Error ("expected invalid-message error, got " ^ Mailer.Delivery.error_to_string error)

let start_supervisor = fun () ->
  match Mailer.Supervisor.start_link () with
  | Ok supervisor -> Ok supervisor
  | Error error -> Error (Mailer.Supervisor.start_error_to_string error)

let test_supervised_mailbox_records_messages = fun _ctx ->
  let* supervisor = start_supervisor () in
  let message =
    Mailer.Message.make ~to_:[ "singer@example.test" ] ~subject:"Supervised" ~text:"Mailbox body" ()
  in
  let* () = expect_delivered (Mailer.deliver_now (Mailer.Supervisor.delivery supervisor) message) in
  match Mailer.Mailbox.list (Mailer.Supervisor.mailbox supervisor) with
  | Error error -> Error (Mailer.Mailbox.error_to_string error)
  | Ok [ delivered ] ->
      let* () = expect (delivered.id = 1) "expected first delivery id" in
      expect_contains delivered.rendered "Subject: Supervised"
  | Ok _ -> Error "expected exactly one supervised delivery"

let test_mailer_routes_show_and_clear_messages = fun _ctx ->
  let* supervisor = start_supervisor () in
  let message =
    Mailer.Message.make
      ~from:"KaraokeCrowd <no-reply@example.test>"
      ~to_:[ "singer@example.test" ]
      ~subject:"Route Visible"
      ~text:"Open https://example.test/api/auth/verify?token=route"
      ()
  in
  let* () = expect_delivered (Mailer.deliver_now (Mailer.Supervisor.delivery supervisor) message) in
  let app = app_with_mailer supervisor in
  let* list_response = expect_response (Testing.App.get app "/__mailbox/messages") in
  let* () = expect_status Net.Http.Status.Ok list_response in
  let* () = expect_contains Response.(list_response.body) "\"subject\":\"Route Visible\"" in
  let* raw_response = expect_response (Testing.App.get app "/__mailbox/messages/1/raw") in
  let* () = expect_status Net.Http.Status.Ok raw_response in
  let* () = expect_contains Response.(raw_response.body) "Subject: Route Visible" in
  let* clear_response = expect_response (Testing.App.delete app "/__mailbox/messages") in
  let* () = expect_status Net.Http.Status.Ok clear_response in
  let* empty_response = expect_response (Testing.App.get app "/__mailbox/messages") in
  let* () = expect_status Net.Http.Status.Ok empty_response in
  expect_contains Response.(empty_response.body) "\"count\":0"

let tests =
  Test.[
    case "Message.render renders multipart text and HTML" test_render_multipart;
    case "Message.render sanitizes header values" test_render_sanitizes_headers;
    case "TestDelivery records delivered messages" test_test_delivery_records_messages;
    case "Delivery.deliver_to_outbox writes .eml files" test_outbox_writes_eml;
    case "Delivery.deliver_now validates recipients" test_invalid_message;
    case "Supervisor mailbox records delivered messages" test_supervised_mailbox_records_messages;
    case "Suri_mailer.routes exposes delivered messages" test_mailer_routes_show_and_clear_messages;
  ]

let main ~args = Test.Cli.main ~name:"suri_mailer_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
