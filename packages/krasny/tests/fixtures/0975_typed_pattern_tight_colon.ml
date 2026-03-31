match value with
| (x : some_really_long_module_path_that_definitely_exceeds_the_line_limit -> another_long_type_name_that_also_forces_a_break -> final_type_name_that_is_still_long) ->
    ()
