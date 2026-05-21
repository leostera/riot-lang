/// The uninhabited type for computations that never produce a value.
type Never

/// The target-sized integer type.
type int

/// Text stored as a runtime string value.
type String

/// Persistent lists of values.
type List<'value> =
  | Nil
  | Cons('value, List<'value>)

/// Optional values.
type Option<'value> = Some('value) | None

/// Fallible results.
type Result<'value, 'err> = Ok('value) | Err('err)

/// Print a debug representation of any boxed runtime value.
external dbg : 'msg -> unit = "riot_rt_dbg_value"

/// Print one string followed by a newline.
external println : String -> unit = "riot_rt_println"

/// Send one message to an actor.
external send : actor_id<'msg> -> 'msg -> unit = "riot_rt_send_value"

/// Monitor an actor and receive a Down message when it exits.
external monitor : actor_id<'msg> -> unit = "riot_rt_monitor"

/// Link the current actor to another actor.
external link : actor_id<'msg> -> unit = "riot_rt_link"

/// Return the length of a list.
external list_len : List<'item> -> i64 = "riot_rt_value_list_len"

/// Return the list item at an index.
external list_get : List<'item> -> i64 -> 'item = "riot_rt_value_list_get"

/// Return the length of a string.
external string_len : String -> i64 = "riot_rt_value_string_len"

/// Concatenate two strings.
external string_concat : String -> String -> String = "riot_rt_value_string_concat"
