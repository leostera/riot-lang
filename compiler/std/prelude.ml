external dbg : 'msg -> unit = "riot_rt_dbg_value"
external println : string -> unit = "riot_rt_println"

external send : actor_id<'msg> -> 'msg -> unit = "riot_rt_send_value"
external monitor : actor_id<'msg> -> unit = "riot_rt_monitor"
external link : actor_id<'msg> -> unit = "riot_rt_link"

external list_len : 'item list -> i64 = "riot_rt_value_list_len"
external list_get : 'item list -> i64 -> 'item = "riot_rt_value_list_get"

external string_len : string -> i64 = "riot_rt_value_string_len"
external string_concat : string -> string -> string = "riot_rt_value_string_concat"
