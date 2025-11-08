open Global

(** # Data.Sexp - S-expression parsing and printing

    A library for working with S-expressions (symbolic expressions), a simple
    and readable data format commonly used in Lisp-like languages and for
    configuration files.

    ## Examples

    Basic parsing and construction:

    ```ocaml open Std.Data

    (* Parse from string *) match Sexp.of_string "(name Alice (age 30))" with |
    Ok sexp -> Sexp.to_string sexp (* "(name Alice (age 30))" *) | Error msg ->
    Log.error "Parse error: %s" msg

    (* Build programmatically *) let person = Sexp.list
    [ Sexp.atom "person"; Sexp.list [Sexp.atom "name"; Sexp.atom "Bob"];
     Sexp.list [Sexp.atom "age"; Sexp.atom "25"] ] in Sexp.to_string person (*
    "(person (name Bob) (age 25))" *) ```

    Extracting data:

    ```ocaml let config = Sexp.of_string "(config (debug true) (port 8080))" |>
    Result.unwrap in

    match Sexp.to_list config with | Some (Atom "config" :: fields) -> (* Look
    up specific field *) (match Sexp.assoc "port" fields with | Some (Atom port)
    -> Printf.printf "Port: %s\n" port | _ -> ()) | _ -> () ```

    Pretty printing:

    ```ocaml let complex = Sexp.list
    [ Sexp.atom "database"; Sexp.list [Sexp.atom "host"; Sexp.atom "localhost"];
     Sexp.list [Sexp.atom "credentials"; Sexp.list [Sexp.atom "user"; Sexp.atom
     "admin"]; Sexp.list [Sexp.atom "password"; Sexp.atom "secret"] ] ] in

    println (Sexp.pretty_print complex) (* Prints with indentation:
    (database (host localhost) (credentials (user admin) (password secret))) *)
    ```

    ## Use Cases

    - Configuration files
    - Data serialization
    - AST representation
    - Protocol messages
    - Simple database formats *)

(** {1 Types} *)

type t =
  | Atom of string
  | List of t list
      (** S-expression representation:
          - [Atom]: A simple string value
          - [List]: A nested list of S-expressions *)

exception Parse_error of string
(** Raised when parsing fails. Contains a description of the error. *)

(** {1 Parsing} *)

val of_string : string -> (t, string) result
(** Parses a string into an S-expression.

    ## Examples

    ```ocaml Sexp.of_string "(hello world)" (* Ok (List
    [Atom "hello"; Atom "world"]) *) Sexp.of_string "atom" (* Ok (Atom "atom")
    *) Sexp.of_string "(unclosed" (* Error "..." *) ```

    ## Syntax

    - Atoms: Any sequence of characters without spaces or parens
    - Lists: Enclosed in parentheses [(...]
    - Whitespace: Separates atoms, ignored otherwise *)

val parse_exn : string -> t
(** Parses a string, raising [Parse_error] on failure.

    ## Examples

    ```ocaml let sexp = Sexp.parse_exn "(foo bar)" in (* Use when you know input
    is valid *) ```

    ## Raises

    [Parse_error] with error message if parsing fails. *)

val parse_many : string -> (t list, string) result
(** Parses multiple S-expressions from a string.

    ## Examples

    ```ocaml match Sexp.parse_many "(first) (second) (third)" with | Ok sexps ->
    List.length sexps (* 3 *) | Error msg -> () ``` *)

val parse_file : string -> (t list, string) result
(** Reads and parses S-expressions from a file.

    ## Examples

    ```ocaml match Sexp.parse_file "config.sexp" with | Ok sexps -> List.iter
    (fun s -> Log.info "Config: %s" (Sexp.to_string s) ) sexps | Error msg ->
    Log.error "Cannot read config: %s" msg ```

    ## Error Cases

    Returns [Error] for:
    - File not found
    - Read errors
    - Parse errors *)

(** {1 Printing} *)

val to_string : t -> string
(** Converts an S-expression to a compact string.

    ## Examples

    ```ocaml let s = Sexp.list [Sexp.atom "a"; Sexp.atom "b"] in Sexp.to_string
    s (* "(a b)" *) ``` *)

val pretty_print : t -> string
(** Pretty-prints an S-expression with indentation for readability.

    ## Examples

    ```ocaml let nested = Sexp.list
    [ Sexp.atom "outer"; Sexp.list [Sexp.atom "inner"; Sexp.atom "value"] ] in

    Sexp.pretty_print nested (* "(outer (inner value))" *) ``` *)

val to_file : string -> t -> (unit, string) result
(** Writes an S-expression to a file.

    ## Examples

    ```ocaml let config = Sexp.list
    [ Sexp.atom "config"; Sexp.list [Sexp.atom "debug"; Sexp.atom "true"] ] in

    match Sexp.to_file "config.sexp" config with | Ok () -> Log.info "Config
    saved" | Error msg -> Log.error "Write failed: %s" msg ``` *)

(** {1 Constructors} *)

val atom : string -> t
(** Creates an atom S-expression.

    ## Examples

    ```ocaml Sexp.atom "hello" (* Atom "hello" *) ``` *)

val list : t list -> t
(** Creates a list S-expression.

    ## Examples

    ```ocaml Sexp.list [Sexp.atom "a"; Sexp.atom "b"] (* List
    [Atom "a"; Atom "b"] *) Sexp.list [] (* List [] - empty list *) ``` *)

(** {1 Accessors} *)

val is_atom : t -> bool
(** Returns [true] if the S-expression is an atom.

    ## Examples

    ```ocaml Sexp.is_atom (Sexp.atom "foo") (* true *) Sexp.is_atom (Sexp.list
    []) (* false *) ``` *)

val is_list : t -> bool
(** Returns [true] if the S-expression is a list.

    ## Examples

    ```ocaml Sexp.is_list (Sexp.list []) (* true *) Sexp.is_list (Sexp.atom
    "foo") (* false *) ``` *)

val to_atom : t -> string option
(** Extracts the string value if the S-expression is an atom.

    ## Examples

    ```ocaml Sexp.to_atom (Sexp.atom "hello") (* Some "hello" *) Sexp.to_atom
    (Sexp.list []) (* None *) ``` *)

val to_list : t -> t list option
(** Extracts the list if the S-expression is a list.

    ## Examples

    ```ocaml let s = Sexp.list [Sexp.atom "a"; Sexp.atom "b"] in Sexp.to_list s
    (* Some [Atom "a"; Atom "b"] *)

    Sexp.to_list (Sexp.atom "foo") (* None *) ``` *)

val find_atom : string -> t list -> t option
(** Searches for an atom by name in a nested structure.

    ## Examples

    ```ocaml let data =
    [ Sexp.list [Sexp.atom "name"; Sexp.atom "Alice"]; Sexp.list [Sexp.atom
     "age"; Sexp.atom "30"] ] in

    Sexp.find_atom "name" data (* Some (List [Atom "name"; Atom "Alice"]) *) ```
*)

val assoc : string -> t list -> t option
(** Association list lookup - finds the value for a key in a list of key-value
    pairs.

    ## Examples

    ```ocaml let pairs =
    [ Sexp.list [Sexp.atom "host"; Sexp.atom "localhost"]; Sexp.list [Sexp.atom
     "port"; Sexp.atom "8080"] ] in

    match Sexp.assoc "port" pairs with | Some (Atom port) -> Printf.printf
    "Port: %s\n" port | _ -> () ``` *)

(** {1 Canonical S-expressions (Csexp)} *)

module Csexp : sig
  val to_string : t -> string
  (** Converts to canonical S-expression format - a length-prefixed, unambiguous
      encoding.

      ## Examples

      ```ocaml let s = Sexp.atom "hello" in Csexp.to_string s (* "5:hello" *)

      let lst = Sexp.list [Sexp.atom "a"; Sexp.atom "b"] in Csexp.to_string lst
      (* "(1:a1:b)" *) ``` *)

  val of_string : string -> (t, string) result
  (** Parses canonical S-expression format.

      ## Examples

      ```ocaml Csexp.of_string "5:hello" (* Ok (Atom "hello") *) Csexp.of_string
      "(1:a1:b)" (* Ok (List [Atom "a"; Atom "b"]) *) ``` *)
end
