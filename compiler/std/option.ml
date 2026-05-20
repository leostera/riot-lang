/// Return true when the option is Some.
external is_some : option<'value> -> bool = "riot_rt_option_is_some"

/// Return true when the option is None.
external is_none : option<'value> -> bool = "riot_rt_option_is_none"

/// Return the contained value or a fallback.
external unwrap_or : option<'value> -> 'value -> 'value = "riot_rt_option_unwrap_or"
