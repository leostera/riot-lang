/// Return true when the result is Ok.
external is_ok : result<'value, 'err> -> bool = "riot_rt_result_is_ok"

/// Return true when the result is Err.
external is_err : result<'value, 'err> -> bool = "riot_rt_result_is_err"

/// Return the contained Ok value or a fallback.
external unwrap_or : result<'value, 'err> -> 'value -> 'value = "riot_rt_result_unwrap_or"
