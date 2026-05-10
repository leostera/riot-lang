(**
   Process-based dynamic event dispatching.

   Inspired by https://hexdocs.pm/telemetry/readme.html

   Start the telemetry server:
   {[
     let _ = Telemetry.start ()
   ]}

   Create events in your library:
   {[
     type Telemetry.event += BuildStarted of { package : string }

     Telemetry.emit (BuildStarted { package = "my-pkg" })
   ]}

   Attach handlers to consume events:
   {[
     Telemetry.attach "my-handler" (fun event ->
         match event with
         | BuildStarted { package } -> Log.info "Building %s" package
         | _ -> ())
   ]}
*)

(**
   Extensible variant type for telemetry events. Any module can extend this
   with their own events.
*)

module Event: sig
  type t = ..
end

type event = Event.t = ..

module Span: sig
  (** A UUIDv7 span identifier. *)
  type id = Uuid.t

  module Attributes: sig
    type 'a key = 'a Collections.TypedKeyHashMap.key

    type binding = Collections.TypedKeyHashMap.binding =
      | Binding: 'a key * 'a -> binding

    type t

    val create: unit -> t

    val key: unit -> 'a key

    val of_list: binding list -> t

    val copy: t -> t

    val get: t -> key:'a key -> 'a option

    val insert: t -> key:'a key -> value:'a -> t

    val remove: t -> key:'a key -> 'a option

    val has_key: t -> key:'a key -> bool

    val length: t -> int

    val is_empty: t -> bool
  end

  (** Extra span metadata stored in a typed-key map. *)
  type attribute = Attributes.binding
  type attributes = Attributes.t

  type status =
    | Succeeded
    | Failed of exn

  type t

  type lifecycle =
    | Started of t
    | Completed of {
        span: t;
        completed_at: Time.Instant.t;
        duration: Time.Duration.t;
        status: status;
      }

  val id: t -> id
  val id_to_string: id -> string
  val equal_id: id -> id -> bool
  val parent_id: t -> id option
  val name: t -> string

  (** Return a copy of the span attributes. *)
  val attributes: t -> attributes

  val get_attribute: t -> key:'a Attributes.key -> 'a option

  val started_at: t -> Time.Instant.t

  (** Start a span and emit a [Started] lifecycle event. *)
  val start: ?span:t -> ?attributes:attributes -> string -> t

  (** Finish a span and emit a [Completed] lifecycle event. *)
  val finish: ?status:status -> t -> unit
end

(** Standard telemetry event emitted by [Telemetry.Span]. *)
type event +=
  | SpanEvent of Span.lifecycle

(**
   Run [fn] inside a telemetry span.

   Pass [?span] to make the new span a child of an existing span. Emits
   [Telemetry.Span.Started] before calling [fn]. Emits
   [Telemetry.Span.Completed] with [Succeeded] when [fn] returns, or [Failed]
   before re-raising when [fn] raises.
*)
val with_span: ?span:Span.t -> ?attributes:Span.attributes -> string -> (Span.t -> 'a) -> 'a

(**
   Start the telemetry server process. Returns the server's PID. Must be called
   before emitting events. Idempotent: returns the existing server PID when the
   server is already running.
*)
val start: unit -> Pid.t

(** Emit a telemetry event. All attached handlers will receive it. *)
val emit: event -> unit

(**
   Attach a named handler for telemetry events. The handler will be called for
   every emitted event. If a handler with the same name already exists, it will
   be replaced.
*)
val attach: string -> (event -> unit) -> unit

(** Detach a handler by name. *)
val detach: string -> unit

(** Detach all handlers. Useful for testing. *)
val detach_all: unit -> unit

(**
   List all attached handler names. Returns [[]] when telemetry is not running
   or when the current server reference is stale.
*)
val list_handlers: unit -> string list

(**
   Stop the telemetry server. Blocks until all pending events are processed.
   This is useful in tests to ensure all events have been handled before making
   assertions. Safe to call multiple times.
*)
val stop: unit -> unit
