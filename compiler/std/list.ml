/// Return the length of a list.
external len : 'item list -> i64 = "riot_rt_value_list_len"

/// Return the list item at an index.
external get : 'item list -> i64 -> 'item = "riot_rt_value_list_get"
