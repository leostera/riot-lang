(** # String - UTF-8 string manipulation

    This module extends OCaml's standard String module with UTF-8 aware
    iteration support. Strings in OCaml are sequences of bytes, and this module
    provides safe UTF-8 character iteration on top of that.

    ## Examples

    Basic string operations:

    ```ocaml open Std

    let text = "Hello, 世界!" in

    (* Standard operations *) let upper = String.uppercase_ascii text in let
    lower = String.lowercase_ascii text in let trimmed = String.trim " spaces "
    in

    (* UTF-8 iteration *) String.into_iter text |> Iterator.for_each (fun char
    -> Printf.printf "Char: %s\n" (Uchar.to_string char)) ```

    ## UTF-8 Support

    While OCaml strings are byte sequences, this module provides:
    - UTF-8 character iteration via [`into_iter`] and [`into_mut_iter`]
    - All standard String module functions for byte-level operations

    ## Common Patterns

    ```ocaml (* Check if string contains substring *) let contains_word text
    word = String.contains text word

    (* Split string into lines *) let lines = String.split_on_char '\n' text

    (* Join strings *) let csv = String.concat "," ["a"; "b"; "c"] ``` *)

include module type of Stdlib.String
(** @inline

    Includes all standard library String functions:

    ## Length and Indexing
    - [`length`] - Get string length in bytes
    - [`get`], [`unsafe_get`] - Get byte at index
    - [`set`], [`unsafe_set`] - Set byte at index (for mutable strings)

    ## Searching
    - [`contains`] - Check if string contains substring
    - [`contains_from`] - Search from position
    - [`index`], [`index_opt`] - Find character position
    - [`index_from`], [`index_from_opt`] - Find from position
    - [`rindex`], [`rindex_opt`] - Find last occurrence
    - [`starts_with`] - Check prefix
    - [`ends_with`] - Check suffix

    ## Transformation
    - [`uppercase_ascii`], [`lowercase_ascii`] - Case conversion
    - [`capitalize_ascii`], [`uncapitalize_ascii`] - First letter case
    - [`trim`] - Remove leading/trailing whitespace
    - [`escaped`] - Escape special characters
    - [`map`], [`mapi`] - Transform characters

    ## Substring Operations
    - [`sub`] - Extract substring
    - [`split_on_char`] - Split on delimiter
    - [`concat`] - Join with separator

    ## Comparison
    - [`compare`] - Lexicographic comparison
    - [`equal`] - Equality check

    ## Conversion
    - [`to_bytes`], [`of_bytes`] - Bytes conversion
    - [`to_seq`], [`to_seqi`], [`of_seq`] - Sequence conversion

    ## Creation
    - [`make`] - Create string of repeated character
    - [`init`] - Create with initialization function
    - [`empty`] - Empty string constant *)

(** # UTF-8 Iteration *)

val into_mut_iter : string -> Uchar.t MutIterator.t
(** Creates a mutable iterator over UTF-8 characters.

    Iterates over Unicode characters (not bytes) in the string. Invalid UTF-8
    sequences are replaced with the replacement character.

    ## Examples

    ```ocaml let text = "Hello, 世界!" in let iter = String.into_mut_iter text in

    (* Count characters (not bytes) *) let char_count = MutIterator.count iter
    in println "Characters: %d" char_count;

    (* Process each character *) String.into_mut_iter text |>
    MutIterator.for_each (fun uchar -> let code = Uchar.to_int uchar in
    Printf.printf "U+%04X " code) ```

    ## Performance

    UTF-8 decoding has some overhead. For byte-level operations, use standard
    String functions instead. *)

val into_iter : string -> Uchar.t Iterator.t
(** Creates an immutable iterator over UTF-8 characters.

    Similar to [`into_mut_iter`] but returns an immutable iterator suitable for
    functional transformations.

    ## Examples

    ```ocaml let text = "Café ☕" in

    (* Filter non-ASCII characters *) let ascii_only = String.into_iter text |>
    Iterator.filter (fun uc -> Uchar.to_int uc < 128) |> Iterator.to_list |>
    List.map Uchar.to_char |> String.of_list in (* ascii_only = "Caf " *)

    (* Count emoji *) let emoji_count = String.into_iter text |> Iterator.filter
    (fun uc -> let code = Uchar.to_int uc in code >= 0x1F600 && code <= 0x1F64F)
    |> Iterator.count ```

    ## UTF-8 Handling

    Invalid UTF-8 sequences are replaced with U+FFFD (�). For strict UTF-8
    validation, check bytes before iteration. *)
