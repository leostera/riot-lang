# Suri Mailer

`suri-mailer` is the Suri companion package for outbound email. It follows the
useful parts of Rails Action Mailer without tying delivery to the web server:

- mailer actions build typed `Message.t` values;
- `Mailer.t` carries defaults such as `from`;
- `MessageDelivery.t` is the deliverable wrapper;
- `Delivery.t` selects how messages are sent;
- local development and tests can use outbox, in-memory, or supervised mailbox
  delivery adapters.

Example:

```ocaml
let delivery = Suri_mailer.Delivery.outbox ~dir:"tmp/mails" ()

let mailer =
  Suri_mailer.Mailer.make
    ~default_from:"KaraokeCrowd <no-reply@example.test>"
    ~delivery
    ()

let welcome_email =
  Suri_mailer.Mailer.mail
    mailer
    ~to_:[ "singer@example.test" ]
    ~subject:"Welcome"
    ~text:"Welcome to KaraokeCrowd"
    ~html:"<p>Welcome to KaraokeCrowd</p>"
    ()

let result = Suri_mailer.MessageDelivery.deliver_now welcome_email
```

For Suri apps that need a test/dev inbox, start the supervised mailbox and mount
its routes:

```ocaml
let mailer =
  Suri_mailer.Supervisor.start_link ()
  |> Result.unwrap

let app =
  Suri.Middleware.[
    router [
      Suri.Middleware.Router.scope "/__suri" [
        Suri.Middleware.Router.forward "/mailer" (Suri_mailer.routes mailer);
      ];
      (* your app routes *)
    ];
  ]

let delivery = Suri_mailer.Supervisor.delivery mailer
```

The mounted scope exposes:

- `GET /__suri/mailer` for a small HTML inbox;
- `GET /__suri/mailer/messages` for JSON summaries;
- `GET /__suri/mailer/messages/:id/json` for JSON details;
- `GET /__suri/mailer/messages/:id/raw` for the rendered `.eml`;
- `DELETE /__suri/mailer/messages` or `POST /__suri/mailer/clear` to clear it.

For local end-to-end tests, use either `Delivery.outbox` when a file watcher is
more convenient or `Supervisor.delivery` when the test harness can inspect the
mounted mailbox routes. `Suri_mailer.Supervisor.routes` remains available when
you want to keep all supervised mailbox calls under the `Supervisor` module.
