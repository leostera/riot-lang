/// Send one message to an actor.
external send : actor_id<'msg> -> 'msg -> unit = "riot_rt_send_value"

/// Monitor an actor and receive a Down message when it exits.
external monitor : actor_id<'msg> -> unit = "riot_rt_monitor"

/// Link the current actor to another actor.
external link : actor_id<'msg> -> unit = "riot_rt_link"
