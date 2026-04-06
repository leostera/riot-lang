let name = "kernel"

let rules = fun () -> [ Prefer_format_over_string_concat.rule () ]

let explanations = fun () -> Prefer_format_over_string_concat.explanations ()
