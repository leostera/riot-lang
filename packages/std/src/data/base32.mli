(** # Data.Base32 - Base32 encoding and decoding

    Base32 encoding converts binary data to a string using 32 printable
    characters (A-Z, 2-7). More compact than Base16 but less efficient than
    Base64. Uses RFC 4648 standard encoding.

    ## Examples

    Basic encoding and decoding:

    ```ocaml open Std.Data

    (* Encode to Base32 *) let encoded = Base32.encode "Hello" in (* "JBSWY3DP"
    *)

    (* Decode from Base32 *) match Base32.decode "JBSWY3DP" with | Ok data -> (*
    "Hello" *) | Error `Invalid_base32 -> () ```

    Encoding bytes:

    ```ocaml let bytes = Bytes.of_string "data" in let encoded =
    Base32.encode_bytes bytes in ```

    ## Use Cases

    - Human-readable identifiers (case-insensitive)
    - TOTP/2FA secret keys
    - Shortened URLs
    - Error-resistant encoding (no ambiguous characters like 0/O, 1/I)
    - Git commit hashes (some systems)

    ## Character Set

    Uses standard Base32 alphabet: A-Z (26 letters) + 2-7 (6 digits) = 32 chars.
    Padding with '=' to make output length a multiple of 8. *)

open Global

(** Encodes a string to Base32.

    ## Examples

    ```ocaml Base32.encode "Hi" (* "JBQQ====" *) Base32.encode "test" (*
    "ORSXG5A=" *) ``` *)
val encode : string -> string

(** Encodes bytes to Base32. *)
val encode_bytes : bytes -> string

(** Decodes a Base32 string. Case-insensitive.

    ## Examples

    ```ocaml Base32.decode "JBSWY3DP" (* Ok "Hello" *) Base32.decode "jbswy3dp"
    (* Ok "Hello" - lowercase works *) Base32.decode "JBSWY3DP====" (* Ok
    "Hello" - padding optional *) Base32.decode "!!!!" (* Error `Invalid_base32
    *) ```

    ## Errors

    Returns [`Invalid_base32] if:
    - String contains invalid Base32 characters
    - Padding is incorrect *)
val decode : string -> (string, [
  | `Invalid_base32
]) result

(** Decodes a Base32 string to bytes. *)
val decode_bytes : string -> (bytes, [
  | `Invalid_base32
]) result
