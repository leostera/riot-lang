open Global

(** # Data.Toml - TOML configuration file parser

    A TOML (Tom's Obvious Minimal Language) parser for configuration files.
    Focuses on simplicity and common use cases for application configuration.

    ## Examples

    Parsing a TOML configuration file:

    ```ocaml open Std.Data

    (* config.toml: name = "my-app" version = "1.0.0" debug = true

    [server] host = "localhost" port = 8080 *)

    match Toml.parse "config.toml" with | Ok root -> (* Extract simple values *)
    let name = Toml.get_string root |> Option.unwrap in

    (* Navigate to nested tables *) let server = Toml.get_table root |>
    Option.and_then (List.assoc_opt "server") |> Option.and_then Toml.get_table
    in

    (match server with | Some fields -> let host = List.assoc_opt "host" fields
    |> Option.and_then Toml.get_string in Printf.printf "Server: %s\n"
    (Option.unwrap host) | None -> ())

    | Error err -> Log.error "TOML parse error: %s" (Toml.error_to_string err)
    ```

    Working with arrays:

    ```ocaml (* config.toml: dependencies = ["foo", "bar", "baz"] *)

    match Toml.parse_file "config.toml" with | Ok root -> let deps =
    Toml.get_table root |> Option.and_then (List.assoc_opt "dependencies") |>
    Option.and_then Toml.get_array in

    (match deps with | Some items -> List.iter (fun item -> match
    Toml.get_string item with | Some dep -> Printf.printf "Dep: %s\n" dep | None
    -> () ) items | None -> ()) | Error err -> () ```

    ## Supported TOML Features

    - Strings
    - Booleans
    - Arrays
    - Tables (sections)

    ## Limitations

    This is a minimal TOML parser focused on common configuration needs. Not all
    TOML 1.0 features are supported:
    - No integers or floats (use strings and convert)
    - No dates/times
    - No inline tables
    - No array of tables

    For full TOML 1.0 support, consider using a more complete parser. *)

(** {1 Types} *)

type value =
  | String of string
  | Int of int
  | Array of value list
  | Table of (string * value) list
  | Bool of bool
      (** TOML value representation supporting strings, integers, booleans, arrays, and
          tables. *)

type error =
  | Invalid_path of { path : string }
  | File_read_error of { path : string; reason : string }
  | Parse_error of { position : int; context : string; reason : string }
  | Unterminated_string of { position : int }
  | Unterminated_array of { position : int }
  | Unexpected_char of { position : int; found : char; expected : string }
      (** TOML parsing errors with position information for debugging. *)

(** {1 Parsing} *)

val parse : string -> (value, error) result
(** Parses a string into TOML and returns the root table.
    
    ## Examples
    
    ```ocaml
    match Toml.parse "<toml ...>" with
    | Ok root ->
        (* Extract configuration *)
        ()
    | Error (File_read_error { path; reason }) ->
        Printf.printf "Cannot read %s: %s\n" path reason
    | Error (Parse_error { position; reason; _ }) ->
        Printf.printf "Parse error at position %d: %s\n" position reason
    | Error err ->
        Log.error "%s" (Toml.error_to_string err)
    ```
    
    ## Error Cases
    
    Returns [Error] for:
    - File not found or not readable
    - Invalid TOML syntax
    - Unterminated strings or arrays
    - Unexpected characters
*)

val error_to_string : error -> string
(** Converts a TOML parse error to a human-readable error message.

    ## Examples

    ```ocaml match Toml.parse "bad.toml" with | Ok _ -> () | Error err ->
    Printf.printf "Error: %s\n" (Toml.error_to_string err) ``` *)

(** {1 Extractors} *)

val get_string : value -> string option
(** Extracts a string value. Returns [None] if not a string.

    ## Examples

    ```ocaml match value with | Toml.String s -> Printf.printf "Got: %s\n" s | _
    -> ()

    (* Or using extractor: *) Toml.get_string value |> Option.iter
    (Printf.printf "Got: %s\n") ``` *)

val get_int : value -> int option
(** Extracts an integer value. Returns [None] if not an integer.

    ## Examples

    ```ocaml match value with | Toml.Int i -> Printf.printf "Port: %d\n" i | _
    -> ()

    (* Or using extractor: *) Toml.get_int value |> Option.iter
    (Printf.printf "Port: %d\n") ``` *)

val get_array : value -> value list option
(** Extracts an array value. Returns [None] if not an array.

    ## Examples

    ```ocaml match Toml.get_array value with | Some items -> List.iter (fun item
    -> match Toml.get_string item with | Some s -> Printf.printf "Item: %s\n" s
    | None -> () ) items | None -> () ``` *)

val get_table : value -> (string * value) list option
(** Extracts a table (section) as a list of key-value pairs. Returns [None] if
    not a table.

    ## Examples

    ```ocaml match Toml.get_table root with | Some fields -> (* Look up a
    specific field *) (match List.assoc_opt "server" fields with | Some
    server_table -> (* process server config *) | None -> Log.warn "No server
    config") | None -> () ``` *)

val to_string : ?indent:int -> value -> string
(** Converts a TOML value to a string representation for debugging.

    ## Examples

    ```ocaml let toml = Toml.Table
    [("name", Toml.String "my-app");  ("debug", Toml.Bool true)] in
    Printf.printf "%s\n" (Toml.to_string toml) ``` *)
