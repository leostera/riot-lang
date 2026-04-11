open Std

(** Structured events emitted by the new `typ` stack.

    These events are write-only telemetry. They can be serialized, but they are
    not part of any deserialization contract.

    The event set is intentionally empty until the new checker layers start
    emitting structured telemetry. *)
type t = |

(** Serialize one event into structured JSON. *)
val to_json: t -> Data.Json.t

(** Serialize one event as a compact newline-delimited JSON record. *)
val to_stream: t -> string
