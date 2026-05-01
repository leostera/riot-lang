type error = Adapter.error =
  | InvalidTimeoutNs of {
      timeout_ns: int64;
    }
  | InvalidMaxEvents of { max_events: int }
  | System of System_error.t

let error_to_string error =
  match error with
  | InvalidTimeoutNs { timeout_ns = _ } -> "invalid async poll timeout"
  | InvalidMaxEvents { max_events } ->
      String.concat "" [ "invalid async max_events: "; Int.to_string max_events ]
  | System system_error -> System_error.to_string system_error

module Token = Token
module Interest = Interest
module Event = Event
module Source = Source
module Poll = Poll
module Adapter = Adapter
