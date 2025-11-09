(** FFI bindings to Rust hello-rust crate *)

external double : int -> int = "Raml_Hello_Double"
external add_ten : int -> int = "Raml_Hello_AddTen"
