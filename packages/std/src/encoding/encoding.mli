(** # Encoding

    Binary-to-text and numeric text encoding helpers.

    The encoding namespace currently exposes:

    - [`Base16`] and [`Hex`] for hexadecimal
    - [`Base32`] for RFC 4648 Base32
    - [`Base64`] for RFC 4648 Base64
    - [`Base85`] for Ascii85/Base85
    - [`Octal`] for octal numeric text encoding and decoding

    ## Example

    ```ocaml
    open Std

    let auth = Encoding.Base64.encode "aladdin:opensesame" in
    let mode = Encoding.Octal.encode_int 0o755 in
    ignore (auth, mode)
    ```
*)

module Base16 = Base16

module Hex = Base16

module Base32 = Base32

module Base64 = Base64

module Base85 = Base85

module Octal = Octal
