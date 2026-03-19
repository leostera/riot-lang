(** FFI bindings to Rust hello-rust crate *)

external double : int -> int = "Riot_Hello_Double"
external add_ten : int -> int = "Riot_Hello_AddTen"
