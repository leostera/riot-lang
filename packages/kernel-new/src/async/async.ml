type error = Adapter.error =
  | System of System_error.t

let error_to_string = function
  | System error -> System_error.to_string error

module Token = Token
module Interest = Interest
module Event = Event
module Source = Source
module Poll = Poll
module Adapter = Adapter
