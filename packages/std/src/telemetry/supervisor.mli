type event = Event.t

val start: unit -> Pid.t

val emit: event -> unit

val attach: string -> (event -> unit) -> unit

val detach: string -> unit

val detach_all: unit -> unit

val list_handlers: unit -> string list

val stop: unit -> unit
