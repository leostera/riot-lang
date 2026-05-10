open Std

module Message: sig
  type t = {
    from: string option;
    to_: string list;
    cc: string list;
    bcc: string list;
    reply_to: string list;
    subject: string;
    headers: (string * string) list;
    text_body: string option;
    html_body: string option;
  }

  val make:
    ?from:string ->
    ?cc:string list ->
    ?bcc:string list ->
    ?reply_to:string list ->
    ?headers:(string * string) list ->
    ?text:string ->
    ?html:string ->
    to_:string list ->
    subject:string ->
    unit ->
    t

  val recipients: t -> string list

  val render: t -> string
end

module Delivery: sig
  type invalid_message =
    | MissingRecipient
    | MissingSubject
    | MissingBody

  type filesystem_error =
    | CreateOutboxDirectoryFailed of {
        path: Path.t;
        reason: string;
      }
    | WriteMessageFailed of {
        path: Path.t;
        reason: string;
      }

  type adapter_error =
    | MailboxNotStarted
    | MailboxTimeout

  type error =
    | InvalidMessage of invalid_message
    | FilesystemError of filesystem_error
    | AdapterError of adapter_error

  type t

  val error_to_string: error -> string

  val adapter: name:string -> (Message.t -> (unit, error) result) -> t

  val name: t -> string

  val deliver_now: t -> Message.t -> (unit, error) result

  val deliver_to_outbox: dir:string -> Message.t -> (Path.t, error) result

  val outbox: dir:string -> unit -> t
end

module Mailbox: sig
  type delivered = {
    id: int;
    delivered_at: string;
    message: Message.t;
    rendered: string;
  }

  type error =
    | NotStarted
    | Timeout
    | StartError of {
        reason: string;
      }

  type t

  val error_to_string: error -> string

  val start_under:
    Std.Supervisor.Dynamic.t ->
    ?max_messages:int ->
    ?request_timeout:Std.Time.Duration.t ->
    unit ->
    (t, error) result

  val deliver: t -> Message.t -> (unit, Delivery.error) result

  val list: t -> (delivered list, error) result

  val get: t -> id:int -> (delivered option, error) result

  val clear: t -> (unit, error) result

  val delivery: t -> Delivery.t
end

module TestDelivery: sig
  type t

  val create_store: unit -> t

  val delivery: t -> Delivery.t

  val deliveries: t -> Message.t list

  val clear: t -> unit
end

module Routes: sig
  val routes: Mailbox.t -> Suri.Middleware.Router.route list
end

module MessageDelivery: sig
  type t

  val make: delivery:Delivery.t -> Message.t -> t

  val message: t -> Message.t

  val deliver_now: t -> (unit, Delivery.error) result
end

module Mailer: sig
  type t

  val make: ?default_from:string -> delivery:Delivery.t -> unit -> t

  val mail:
    t ->
    ?from:string ->
    ?cc:string list ->
    ?bcc:string list ->
    ?reply_to:string list ->
    ?headers:(string * string) list ->
    ?text:string ->
    ?html:string ->
    to_:string list ->
    subject:string ->
    unit ->
    MessageDelivery.t
end

module Supervisor: sig
  type t

  type start_error = Mailbox.error

  val start_error_to_string: start_error -> string

  val start_link:
    ?max_messages:int ->
    ?request_timeout:Std.Time.Duration.t ->
    unit ->
    (t, start_error) result

  val dynamic_supervisor: t -> Std.Supervisor.Dynamic.t

  val mailbox: t -> Mailbox.t

  val delivery: t -> Delivery.t

  val mailer: ?default_from:string -> t -> Mailer.t

  val routes: t -> Suri.Middleware.Router.route list
end

val routes: Supervisor.t -> Suri.Middleware.Router.route list

val deliver_now: Delivery.t -> Message.t -> (unit, Delivery.error) result
