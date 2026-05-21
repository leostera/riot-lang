/// Return the length of a list.
external len : List<'item> -> i64 = "riot_rt_value_list_len"

/// Return the list item at an index.
external get : List<'item> -> i64 -> 'item = "riot_rt_value_list_get"
