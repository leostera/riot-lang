(** # Data.Base85 - Base85 (Ascii85) encoding and decoding
    
    Base85 encoding converts binary data to ASCII text using 85 printable 
    characters. More space-efficient than Base64 (4 bytes → 5 chars vs 
    4 bytes → 6 chars in Base64).
    
    Uses the Ascii85 variant popularized by PostScript and PDF.
    
    ## Examples
    
    Basic encoding and decoding:
    
    ```ocaml
    open Std.Data
    
    (* Encode to Base85 *)
    let encoded = Base85.encode "Hello" in
    (* "87cURD]i" *)
    
    (* Decode from Base85 *)
    match Base85.decode "87cURD]i" with
    | Ok data -> (* "Hello" *)
    | Error `Invalid_base85 -> ()
    ```
    
    Special encoding for sequences of zeros:
    
    ```ocaml
    (* Four zero bytes are encoded as 'z' *)
    Base85.encode "\x00\x00\x00\x00test"
    (* "z@:E^" *)
    ```
    
    ## Use Cases
    
    - PDF embedded data
    - PostScript files
    - Git binary diffs
    - IPv6 address representation (RFC 1924)
    - Compact binary-to-text encoding
    
    ## Character Set
    
    Uses characters from '!' (33) to 'u' (117) in ASCII, plus special 'z' 
    for all-zero groups. Total: 85 characters.
*)

val encode : string -> string
(** Encodes a string to Ascii85.

    ## Examples

    ```ocaml Base85.encode "Man" (* "9jqo^" *) Base85.encode "\x00\x00\x00\x00"
    (* "z" - special case *) ``` *)

val encode_bytes : bytes -> string
(** Encodes bytes to Ascii85. *)

val decode : string -> (string, [ `Invalid_base85 ]) result
(** Decodes an Ascii85 string.

    ## Examples

    ```ocaml Base85.decode "9jqo^" (* Ok "Man" *) Base85.decode "z" (* Ok
    "\x00\x00\x00\x00" *) Base85.decode "<~9jqo^~>" (* Ok "Man" - handles
    delimiters *) ```

    ## Errors

    Returns [`Invalid_base85] if:
    - String contains invalid characters
    - Encoding is malformed

    ## Notes

    - Handles optional `<~` and `~>` delimiters used in PDF/PostScript
    - 'z' expands to four zero bytes *)

val decode_bytes : string -> (bytes, [ `Invalid_base85 ]) result
(** Decodes an Ascii85 string to bytes. *)
