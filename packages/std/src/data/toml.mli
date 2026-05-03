open Global

(**
   TOML parsing and serialization.

   A small TOML parser for configuration text. It supports strings, integers,
   booleans, arrays, tables, and inline tables.

   ## Examples

   Parsing TOML source text:

   ```ocaml
   open Std.Data

   let source =
     {|
     name = "my-app"
     version = 1
     debug = true

     [server]
     host = "localhost"
     port = 8080
     |}
   in

   match Toml.parse source with
   | Ok root ->
       let server =
         Toml.get_table root
         |> Option.and_then (List.assoc_opt "server")
         |> Option.and_then Toml.get_table
       in
       (match server with
       | Some fields ->
           let host = List.assoc_opt "host" fields |> Option.and_then Toml.get_string in
           Printf.printf "Server: %s\n" (Option.unwrap host)
       | None -> ())
   | Error err ->
       Log.error "TOML parse error: %s" (Toml.error_to_string err)
   ```

   Working with arrays:

   ```ocaml
   let source = {|dependencies = ["foo", "bar", "baz"]|} in

   match Toml.parse source with
   | Ok root ->
       let deps =
         Toml.get_table root
         |> Option.and_then (List.assoc_opt "dependencies")
         |> Option.and_then Toml.get_array
       in
       (match deps with
       | Some items ->
           List.iter
             (fun item ->
               match Toml.get_string item with
               | Some dep -> Printf.printf "Dep: %s\n" dep
               | None -> ())
             items
       | None -> ())
   | Error _ ->
       ()
   ```

   ## Supported TOML Features

   - Strings
   - Integers
   - Booleans
   - Arrays
   - Tables (sections)
   - Inline tables

   ## Limitations

   This is a minimal TOML parser focused on common configuration needs. Not all
   TOML 1.0 features are supported:
   - No floats
   - No dates/times
   - No array of tables

   For full TOML 1.0 support, consider using a more complete parser.
*)

(**
   TOML value representation supporting strings, integers, booleans, arrays,
   and tables.
*)
type value =
  | String of string
  | Int of int
  | Array of value list
  | Table of (string * value) list
  | Bool of bool
(** TOML parsing errors with position information for debugging. *)
type error =
  | Invalid_path of { path: string }
  | File_read_error of { path: string; reason: string }
  | Parse_error of { position: int; context: string; reason: string }
  | Unterminated_string of { position: int }
  | Unterminated_array of { position: int }
  | Unexpected_char of { position: int; found: char; expected: string }

(**
   Parses TOML source text and returns the root table.

   ## Examples

   ```ocaml
   match Toml.parse {|name = "my-app"|} with
   | Ok root ->
       (* Extract configuration *)
       ()
   | Error (Parse_error { position; reason; _ }) ->
       Printf.printf "Parse error at position %d: %s\n" position reason
   | Error err ->
       Log.error "%s" (Toml.error_to_string err)
   ```

   ## Error Cases

   Returns [Error] for invalid TOML syntax, unterminated strings or arrays,
   and unexpected characters.
*)
val parse: string -> (value, error) result

(**
   Converts a TOML parse error to a human-readable error message.

   ## Examples

   ```ocaml match Toml.parse "bad.toml" with | Ok _ -> () | Error err ->
   Printf.printf "Error: %s\n" (Toml.error_to_string err) ```
*)
val error_to_string: error -> string

(**
   Extracts a string value. Returns [None] if not a string.

   ## Examples

   ```ocaml match value with | Toml.String s -> Printf.printf "Got: %s\n" s | _
   -> ()

   (* Or using extractor: *) Toml.get_string value |> Option.iter
   (Printf.printf "Got: %s\n") ```
*)
val get_string: value -> string option

(**
   Extracts an integer value. Returns [None] if not an integer.

   ## Examples

   ```ocaml match value with | Toml.Int i -> Printf.printf "Port: %d\n" i | _
   -> ()

   (* Or using extractor: *) Toml.get_int value |> Option.iter
   (Printf.printf "Port: %d\n") ```
*)
val get_int: value -> int option

(**
   Extracts an array value. Returns [None] if not an array.

   ## Examples

   ```ocaml match Toml.get_array value with | Some items -> List.iter (fun item
   -> match Toml.get_string item with | Some s -> Printf.printf "Item: %s\n" s
   | None -> () ) items | None -> () ```
*)
val get_array: value -> value list option

(**
   Extracts a table (section) as a list of key-value pairs. Returns [None] if
   not a table.

   ## Examples

   ```ocaml match Toml.get_table root with | Some fields -> (* Look up a
   specific field *) (match List.assoc_opt "server" fields with | Some
   server_table -> (* process server config *) | None -> Log.warn "No server
   config") | None -> () ```
*)
val get_table: value -> (string * value) list option

(**
   Converts a TOML value to a string representation for debugging.

   ## Examples

   ```ocaml let toml = Toml.Table
   [("name", Toml.String "my-app");  ("debug", Toml.Bool true)] in
   Printf.printf "%s\n" (Toml.to_string toml) ```
*)
val to_string: ?indent:int -> value -> string
