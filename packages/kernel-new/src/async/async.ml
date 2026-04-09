type error = Adapter.error =
  | Invalid_timeout_ns of { timeout_ns: int64 }
  | Invalid_max_events of { max_events: int }
  | System of System_error.t

let error_to_string = function
  | Invalid_timeout_ns { timeout_ns=_ } -> "invalid async poll timeout"
  | Invalid_max_events { max_events } -> String.concat
    ""
    [ "invalid async max_events: "; Int.to_string max_events ]
  | System error -> System_error.to_string error

module Token = Token
module Interest = Interest
module Event = Event
module Source = Source
module Poll = Poll
module Adapter = Adapter
