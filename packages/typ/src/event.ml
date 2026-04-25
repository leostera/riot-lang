open Std

type t =
  | TypingStarted of { started_at: Time.Instant.t }
  | TypingFinished of { finished_at: Time.Instant.t }

let instant_serializer =
  Serde.Ser.contramap
    (
      fun (_instant: Time.Instant.t) -> ()
    )
    Serde.Ser.null

let serializer =
  Serde.Ser.variant
    [
      Serde.Ser.Variant.newtype "TypingStarted" instant_serializer
        (
          function
          | TypingStarted { started_at } -> Some started_at
          | TypingFinished _ -> None
        );
      Serde.Ser.Variant.newtype "TypingFinished" instant_serializer
        (
          function
          | TypingFinished { finished_at } -> Some finished_at
          | TypingStarted _ -> None
        );
    ]
