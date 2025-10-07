(** # Buffer - Growable string buffers

    Mutable string buffers for efficient string concatenation. This is a
    re-export of OCaml's [Stdlib.Buffer] module.

    ## Examples

    Basic string building:

    ```ocaml open Std

    let buf = Buffer.create 16 in Buffer.add_string buf "Hello"; Buffer.add_char
    buf ' '; Buffer.add_string buf "World"; let result = Buffer.contents buf (*
    "Hello World" *) ```

    Building strings in loops:

    ```ocaml let numbers = [1; 2; 3; 4; 5] in let buf = Buffer.create 32 in

    List.iter (fun n -> Buffer.add_string buf (string_of_int n); Buffer.add_char
    buf ',' ) numbers;

    Buffer.contents buf (* "1,2,3,4,5," *) ```

    ## When to Use Buffer

    - Building strings incrementally (O(1) amortized append)
    - Accumulating output in loops
    - String formatting with many concatenations

    ## When to Use Alternatives

    - Simple concatenation → Use [(^)] operator
    - Formatting with placeholders → Use [Printf.sprintf] or [format]
    - Joining list of strings → Use [String.concat]

    ## Performance

    Buffer is much more efficient than repeated string concatenation:

    ```ocaml (* SLOW - O(n²) due to string immutability *) let slow strings =
    List.fold_left (fun acc s -> acc ^ s) "" strings

    (* FAST - O(n) with Buffer *) let fast strings = let buf = Buffer.create 256
    in List.iter (Buffer.add_string buf) strings; Buffer.contents buf ```

    ## See Also

    - [String] for string operations
    - [Printf] for formatted output
    - Full documentation at: https://ocaml.org/api/Buffer.html *)

include module type of Stdlib.Buffer
