open Std

(** Parse Server-Sent Events from an HTTP response stream. *)
(** An SSE event. *)
type event = {
  (** Event payload. *)
  data: string;
  (** Optional [`event:`] field. *)
  event_type: string option;
  (** Optional [`id:`] field. *)
  id: string option;
}

(** Return a mutable iterator over SSE events from the connection.

    The iterator parses [`data:`], [`event:`], and [`id:`] fields lazily as
    the response stream advances.

    ```ocaml
    Blink.SSE.await conn
    |> Iter.MutIterator.for_each (fun event ->
         Log.info event.data)
    ```
*)
val await: Connection.t -> event Iter.MutIterator.t
