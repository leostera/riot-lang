(** Configuration module for Unicode processing *)
type t = { mutable east_asian_width: bool }

(** Whether to treat ambiguous-width characters as double-width (for East Asian locales) *)
let state = { east_asian_width = false }

let set_east_asian_width = fun b -> state.east_asian_width <- b

let get_east_asian_width = fun () -> state.east_asian_width
