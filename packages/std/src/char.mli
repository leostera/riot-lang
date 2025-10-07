(** # Char - Character operations

    Operations on single characters (8-bit ASCII/Latin-1). This is a re-export
    of OCaml's [Stdlib.Char] module.

    ## Examples

    Character testing:

    ```ocaml open Std

    Char.is_alpha 'a' (* true *) Char.is_digit '5' (* true *) Char.is_whitespace
    ' ' (* true *) Char.is_uppercase 'A' (* true *) ```

    Case conversion:

    ```ocaml Char.uppercase 'a' (* 'A' *) Char.lowercase 'Z' (* 'z' *) ```

    ## Note on Unicode

    This module operates on single bytes (0-255), not Unicode code points. For
    Unicode text processing, use [String] which supports UTF-8.

    ## See Also

    - [String] for UTF-8 string operations
    - Full documentation at: https://ocaml.org/api/Char.html *)

include module type of Stdlib.Char
