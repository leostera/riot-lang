open Std

type t =
  | TypingStarted of { started_at: Time.Instant.t }
  | TypingFinished of { finished_at: Time.Instant.t }
val serializer: t Serde.Ser.t
