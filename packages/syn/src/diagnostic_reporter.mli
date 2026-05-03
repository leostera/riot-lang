open Std

(* Diagnostic reporting module for pretty-printing parse errors with source context *)

(* Print diagnostics in a human-readable format with source context.

   Example output:
   ```
   ./file.ml
   001 | type '(* hello *)a
       |       ^ expected type variable name but found comment
       |
   003 | type ' a
       |       ^ expected type variable name but found whitespace
       |
   ```

   **Parameters:**
   - `file` - Path to the source file
   - `source` - The source code content
   - `diagnostics` - List of diagnostics to report
*)
val print: file:string -> source:string -> Diagnostic.t list -> unit

val format: file:string -> source:string -> Diagnostic.t list -> string
