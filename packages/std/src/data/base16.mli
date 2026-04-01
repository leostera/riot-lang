(** # Data.Base16 - Hexadecimal encoding and decoding

    Base16 (hexadecimal) encoding converts binary data to a string of
    hexadecimal digits (0-9, A-F). Each byte is represented by two hex digits.

    ## Examples

    Basic encoding and decoding:

    ```ocaml open Std.Data

    (* Encode to hex *) let hex = Base16.encode "Hello" in (* "48656C6C6F" *)

    (* Decode from hex *) match Base16.decode "48656C6C6F" with | Ok data -> (*
    "Hello" *) | Error `Invalid_base16 -> () ```

    With lowercase output:

    ```ocaml let hex = Base16.encode_lower "Hello" in (* "48656c6c6f" *) ```

    Encoding bytes:

    ```ocaml let bytes = Bytes.of_string "data" in let hex = Base16.encode_bytes
    bytes in (* "64617461" *) ```

    ## Use Cases

    - Displaying binary data in readable form
    - Color codes (#FF0000)
    - Checksums and hashes (MD5, SHA)
    - Network protocols (MAC addresses)
    - Debugging binary data *)

open Global
(** Encodes a string to uppercase hexadecimal.

    ## Examples

    ```ocaml Base16.encode "Hi" (* "4869" *) Base16.encode "\x00\xFF" (* "00FF"
    *) ``` *)
val encode: string -> string
(** Encodes a string to lowercase hexadecimal.

    ## Examples

    ```ocaml Base16.encode_lower "Hi" (* "4869" - wait this is still uppercase
    for digits *) Base16.encode_lower "\xAB\xCD" (* "abcd" *) ``` *)
val encode_lower: string -> string
(** Encodes bytes to uppercase hexadecimal.

    ## Examples

    ```ocaml let b = Bytes.of_string "test" in Base16.encode_bytes b (*
    "74657374" *) ``` *)
val encode_bytes: bytes -> string
(** Encodes bytes to lowercase hexadecimal. *)
val encode_bytes_lower: bytes -> string
(** Decodes a hexadecimal string. Accepts both uppercase and lowercase.

    ## Examples

    ```ocaml Base16.decode "4869" (* Ok "Hi" *) Base16.decode "48 69" (* Error
    `Invalid_base16 - spaces not allowed *) Base16.decode "4G" (* Error
    `Invalid_base16 - G not valid hex *) ```

    ## Errors

    Returns [`Invalid_base16] if:
    - String length is not even
    - String contains non-hexadecimal characters *)
val decode: string -> (string, [
    | `Invalid_base16
  ]) result
(** Decodes a hexadecimal string to bytes. *)
val decode_bytes: string -> (bytes, [
    | `Invalid_base16
  ]) result
