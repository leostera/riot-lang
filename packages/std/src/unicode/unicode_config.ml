(** Configuration module for Unicode processing *)
open Global

(** Whether to treat ambiguous-width characters as double-width (for East Asian locales) *)
let east_asian_width = ref false

let set_east_asian_width = fun b -> east_asian_width := b

let get_east_asian_width = fun () -> !east_asian_width
